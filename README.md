# Open Quran Server

This is a very ambitious project to create a very high performing
webserver to serve Quran texts.

It is implemented after design goals:

* everything from memory (no database, and therefore VERY fast)
* written in Haskell (perfect tool for the job)
* understands reference queries (like: 1:1-4,2:256)
* adheres to the [Quranic Data Formats](https://github.com/oqc/qdf)
* serves JSON for consumption by all
* open source (AGPLv3)

This project is very much a work in progress.


## TODO

* Go Yesod...
* Create query handler :)
* Make Yesod to accept pings from github post commits hooks
* On ping try to (re)load all QTF and QPF files
* Keep all used URLs (repos and files) up-to-date in a plaintext file,
  for "rebooting" or "total refresh"
* Make local backups
* Add metadata to JSON output (low)


## License

AGPLv3.


