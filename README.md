nvWikiLink
==========

Universal system for creating plain text wiki links to nvALT notes


### About ###

Takes input via STDIN and/or arguments and scans for WikiLinks or [[wiki links]]. If found, it looks for a matching note based on the text, breaking CamelCase apart. If no viable match is found, it creates a new note. Any additional input passed is added to the new note. nvWikiLinker requires that your nvALT notes be stored as text files on the disk.

### Configuration ###

Be sure to edit the line near the top:

    $notes_path = '~/Dropbox/nvALT2.2/'

to point to the folder where you keep your NV/nvALT notes.

### Usage ###

The ideal way to run it is as a system service (built with Automator). Create a new Service and add the "Run shell script" action to it. Set up Service receives selected: "text" in "any application" and make sure "Output replaces selected text" is disabled. Select /usr/bin/ruby as the interpreter and paste the contents of this script into it. Set  "Pass input:" to "to STDIN." Save it as "Open nvALT WikiLink" and it will be available in your Services menu (right click or application menu).
