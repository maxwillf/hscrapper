INSTALLING `hscrapper`
---------------
First, get the source files from:

https://github.com/maxwillf/hscrapper

`hscrapper` is written in Haskell and uses the
[stack](http://docs.haskellstack.org/en/stable/README.html) tool. Once
`stack` is installed, move to the source directory and perform:

    stack install

USAGE
-----
Assuming you've made it so that you can run the executable, the following
command-line options are available:

```
Usage: hscrapper (-i| --inputFile InputConfigFile)
                 (-o| --outputDir OutputDirectory)
Available options:
  -i,--inputFile InputConfigFile
                           Input Config file needed for scrapping
  -o,--outputDir OutputDirectory
                           Output Directory
  -h,--help                Show this help text
```

EXPECTED INPUT FILE FORMAT
-----
A scrapping config file consists of one or more configs like the ones below.

*domain* is the site domain so that anchors with incomplete links can be corrected and properly acessed (like anchors that reference filePaths local to the remote website http server )


*startingPage* is, as the name suggests, the page where the *hookItens* will be scrapped so that they can be used as sources for the *contentItens*. If there aren't any hookItens the startingPage will be used as the source for scrapping of its contentItens.

*hookItens* describes where to look for potencial anchors to be visited and eventually scrapped.

*contentItens* describes where to look for the true contents you want  to be scrapped and written to the outputFile. If it's empty then the entire page will be considered important.

```
- domain: https://www.sitedomain.com/
  startingPage: https://www.sitedomain.com/good-scrapping
  hookItens:
    - tag: h3
      attr:
        - attrName: class
          attrVal: entry_title
  contentItens:
    - tag: article
      attr:
        - attrName: class
          attrVal: post
  outputFilename: scrapped.html

- domain: https://www.sitedomain.com/
  startingPage: https://www.sitedomain.com/single-page-scrap
  contentItens:
    - tag: li
      attr:
        - attrName: class
          attrVal: important-information
  outputFilename: target.html
```