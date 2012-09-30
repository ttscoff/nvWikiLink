#!/usr/bin/env ruby
# nvWikiLinker by Brett Terpstra (2012)
# Distribution unrestricted, attribution encouraged

#-------------------------- ABOUT -----------------------------------------
=begin
Takes input via STDIN and/or arguments and scans for WikiLinks or
[[wiki links]]. If found, it looks for a matching note based on the text,
breaking CamelCase apart. If no viable match is found, it creates a new
note. Any additional input passed is added to the new note. nvWikiLinker
requires that your nvALT notes be stored as text files on the disk.

The ideal way to run it is as a system service (built with Automator).
Create a new Service and add the "Run shell script" action to it. Set
up Service receives selected: "text" in "any application" and make sure
"Output replaces selected text" is disabled. Select /usr/bin/ruby as the
interpreter and paste the contents of this script into it. Set  "Pass
input:" to "to STDIN." Save it as "Open nvALT WikiLink" and it will be
available in your Services menu (right click or application menu).
=end
#------------------------ CONFIG -----------------------------------------
# Configure the path to your nvALT notes.
$notes_path = '~/Dropbox/nvALT2.2/'
#---------------------- END CONFIG. That was easy, right? ----------------

require 'cgi'
class String
  # convert "WikiLink" to "Wiki link"
  def break_camel
    return downcase if match(/\A[A-Z]+\z/)
    gsub(/([A-Z]+)([A-Z][a-z])/, '\1 \2').
    gsub(/([a-z])([A-Z])/, '\1 \2').
    downcase
  end
end

class WikiLinker
  def run(input, args)
    unless input.nil? # if there's STDIN input (piped or from Service)
      input.strip!
      # if there are command line arguments, use them as the title
      if args.length > 0
        firstmatch = args
      else
        # Search input for [[style links]]
        firstmatch = input.match(/\[\[(.*?)\]\]/)
        if firstmatch.nil?
          # no [[links]], look for WikiLinks
          firstmatch = input.match(/([A-Z][a-z]+)([A-Z][a-z]*)+/)
          if firstmatch.nil? # nothing found, exit
            $stderr.puts "No wiki links found in text"; exit 1
          else # found WikiLink
            firstmatch = firstmatch[0]
          end
        else
          # found [[link]]
          firstmatch = firstmatch[1]
        end
      end
    else
      exit 0
    end

    finder = FuzzyFileFinder.new($notes_path)

    # search the notes folder for a filename matching title (score >= 0.2)
    res = finder.find(firstmatch.break_camel)
    # sort matches by score and reject if high score < 0.2

    topmatch = nil
    unless res.nil?
      res.sort! {|a,b|
        a[:score] <=> b[:score]
      }

      valid_matches = []
      res.map {|match|
        valid_matches.push(match[:path]) if match[:score] >= 0.2
      }
      valid_matches.reverse!

      unless valid_matches.empty?
        topmatch = valid_matches[0]
      end

    end

    if topmatch.nil?
      # no matches, create a new note
      txt = CGI.escapeHTML(input.sub(/\[\[(.*?)\]\]/,"\\1"))
      title = CGI.escapeHTML(firstmatch.break_camel.capitalize)

      txt = "New note" if txt == ''
      note_url = "nvalt://make/?title=#{title}&txt=#{txt}"
      %x{open "#{note_url}"}
    else
      # match found, open note
      system("open", topmatch, "-a", "nvALT")
    end

    exit 0
  end

end

#--
# ==================================================================
# Author: Jamis Buck (jamis@jamisbuck.org)
# Date: 2008-10-09
#
# This file is in the public domain. Usage, modification, and
# redistribution of this file are unrestricted.
# ==================================================================
#++

class FuzzyFileFinder
  module Version
    MAJOR = 1
    MINOR = 0
    TINY  = 4
    STRING = [MAJOR, MINOR, TINY].join(".")
  end

  class TooManyEntries < RuntimeError; end

  class CharacterRun < Struct.new(:string, :inside) #:nodoc:
    def to_s
      if inside
        "(#{string})"
      else
        string
      end
    end
  end

  class FileSystemEntry #:nodoc:
    attr_reader :parent
    attr_reader :name

    def initialize(parent, name)
      @parent = parent
      @name = name
    end

    def path
      File.join(parent.name, name)
    end
  end

  class Directory #:nodoc:
    attr_reader :name

    def initialize(name, is_root=false)
      @name = name
      @is_root = is_root
    end

    def root?
      is_root
    end
  end

  attr_reader :roots
  attr_reader :files
  attr_reader :ceiling
  attr_reader :shared_prefix
  attr_reader :ignores


  def initialize(directories=['.'], ceiling=10_000, ignores=nil)
    directories = Array(directories)
    directories << "." if directories.empty?

    root_dirnames = directories.map { |d| File.expand_path(d) }.select { |d| File.directory?(d) }.uniq

    @roots = root_dirnames.map { |d| Directory.new(d, true) }
    @shared_prefix = determine_shared_prefix
    @shared_prefix_re = Regexp.new("^#{Regexp.escape(shared_prefix)}" + (shared_prefix.empty? ? "" : "/"))

    @files = []
    @ceiling = ceiling

    @ignores = Array(ignores)

    rescan!
  end

  def rescan!
    @files.clear
    roots.each { |root| follow_tree(root) }
  end

  def search(pattern, &block)
    pattern.gsub!(" ", "")
    path_parts = pattern.split("/")
    path_parts.push "" if pattern[-1,1] == "/"

    file_name_part = path_parts.pop || ""

    if path_parts.any?
      path_regex_raw = "^(.*?)" + path_parts.map { |part| make_pattern(part) }.join("(.*?/.*?)") + "(.*?)$"
      path_regex = Regexp.new(path_regex_raw, Regexp::IGNORECASE)
    end

    file_regex_raw = "^(.*?)" << make_pattern(file_name_part) << "(.*)$"
    file_regex = Regexp.new(file_regex_raw, Regexp::IGNORECASE)

    path_matches = {}
    files.each do |file|
      path_match = match_path(file.parent, path_matches, path_regex, path_parts.length)
      next if path_match[:missed]

      match_file(file, file_regex, path_match, &block)
    end
  end

  def find(pattern, max=nil)
    results = []
    search(pattern) do |match|
      results << match
      break if max && results.length >= max
    end
    return results
  end

  def inspect #:nodoc:
    "#<%s:0x%x roots=%s, files=%d>" % [self.class.name, object_id, roots.map { |r| r.name.inspect }.join(", "), files.length]
  end

  private

    def follow_tree(directory)
      Dir.entries(directory.name).each do |entry|
        next if entry[0,1] == "."
        next if ignore?(directory.name)
        raise TooManyEntries if files.length > ceiling

        full = File.join(directory.name, entry)

        if File.directory?(full)
          follow_tree(Directory.new(full))
        elsif !ignore?(full.sub(@shared_prefix_re, ""))
          files.push(FileSystemEntry.new(directory, entry))
        end
      end
    end

    def ignore?(name)
      ignores.any? { |pattern| File.fnmatch(pattern, name) }
    end

    def make_pattern(pattern)
      pattern = pattern.split(//)
      pattern << "" if pattern.empty?

      pattern.inject("") do |regex, character|
        regex << "([^/]*?)" if regex.length > 0
        regex << "(" << Regexp.escape(character) << ")"
      end
    end

    def build_match_result(match, inside_segments)
      runs = []
      inside_chars = total_chars = 0
      match.captures.each_with_index do |capture, index|
        if capture.length > 0

          inside = index % 2 != 0

          total_chars += capture.gsub(%r(/), "").length
          inside_chars += capture.length if inside

          if runs.last && runs.last.inside == inside
            runs.last.string << capture
          else
            runs << CharacterRun.new(capture, inside)
          end
        end
      end

      inside_runs = runs.select { |r| r.inside }
      run_ratio = inside_runs.length.zero? ? 1 : inside_segments / inside_runs.length.to_f

      char_ratio = total_chars.zero? ? 1 : inside_chars.to_f / total_chars

      score = run_ratio * char_ratio

      return { :score => score, :result => runs.join }
    end

    def match_path(path, path_matches, path_regex, path_segments)
      return path_matches[path] if path_matches.key?(path)

      name_with_slash = path.name + "/"
      matchable_name = name_with_slash.sub(@shared_prefix_re, "")
      matchable_name.chop!

      if path_regex
        match = matchable_name.match(path_regex)

        path_matches[path] =
          match && build_match_result(match, path_segments) ||
          { :score => 1, :result => matchable_name, :missed => true }
      else
        path_matches[path] = { :score => 1, :result => matchable_name }
      end
    end

    def match_file(file, file_regex, path_match, &block)
      if file_match = file.name.match(file_regex)
        match_result = build_match_result(file_match, 1)
        full_match_result = path_match[:result].empty? ? match_result[:result] : File.join(path_match[:result], match_result[:result])
        shortened_path = path_match[:result].gsub(/[^\/]+/) { |m| m.index("(") ? m : m[0,1] }
        abbr = shortened_path.empty? ? match_result[:result] : File.join(shortened_path, match_result[:result])

        result = { :path => file.path,
                   :abbr => abbr,
                   :directory => file.parent.name,
                   :name => file.name,
                   :highlighted_directory => path_match[:result],
                   :highlighted_name => match_result[:result],
                   :highlighted_path => full_match_result,
                   :score => path_match[:score] * match_result[:score] }
        yield result
      end
    end

    def determine_shared_prefix
      return roots.first.name if roots.length == 1

      split_roots = roots.map { |root| root.name.split(%r{/}) }
      segments = split_roots.map { |root| root.length }.max
      master = split_roots.pop

      segments.times do |segment|
        if !split_roots.all? { |root| root[segment] == master[segment] }
          return master[0,segment].join("/")
        end
      end

      return roots.first.name
    end
end

input = ''
input = STDIN.read if STDIN.stat.size > 0
WikiLinker.new.run(input,ARGV.join(' '))
