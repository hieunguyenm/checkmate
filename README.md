# checkmate

Checkmate is a checksum-aware static content webserver.

It serves all files located in a folder called `files`, identified by checksums of the files. Checkmate also watches the folder for any changes and updates the checksum mapping if the file content has changed, or adds a new checksum if the file is new.

A file can be accessed on the webserver by navigating to the SHA-256 checksum as the URL path.

For example, given a file with the checksum:

```
566f4673da89676240ea0f45114f88903d4bcdcbdaeabd65dfa96510c4628362
```

The file can be accessed at:

```
https://your.domain.here/566f4673da89676240ea0f45114f88903d4bcdcbdaeabd65dfa96510c4628362
```

## Running the webserver

Make sure to have `cabal` installed and run:

```
cabal v2-run
```
