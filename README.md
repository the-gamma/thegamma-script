# The Gamma: Tools for open data-driven storytelling

[![Build status](https://api.travis-ci.org/the-gamma/thegamma-script.svg)](https://travis-ci.org/the-gamma/thegamma-script)
[![NPM version](https://img.shields.io/npm/v/thegamma-script.svg)](https://npmjs.org/package/thegamma-script)

<img align="right" src="https://github.com/the-gamma/thegamma-script/raw/master/img/logo.png" alt="The Gamma logo" />

The Gamma is a simple JavaScript library that lets anyone create transparent and open data 
visualizations that are linked to the original data source and encourage the reader to 
further explore data and find interesting facts on their own.

The Gamma implements a scripting language with spreadsheet-inspired tooling 
that runs in the browser and lets users perform simple data aggregation and exploration. 
The package lets you run The Gamma scripts and provides a rich web-based editor that 
you can embed on your site. 

## The Gamma in action

 - [Project homepage](http://thegamma.net/) contains all you need to get started
 - [The Gamma sample web](http://thegamma-sample-web.azurewebsites.net/) shows a minimal demo ([source](https://github.com/the-gamma/thegamma-sample-web))
 - [Visualization of Olympic medals](http://rio2016.thegamma.net/) is bigger sample project ([source](https://github.com/the-gamma/thegamma-olympics-web))
 
## The Gamma script

The core of The Gamma project is a simple scripting langauge that makes it easy to write
code to perform data aggregation and data exploration. For example, if you want to find the
top 8 athletes by the number of gold medals from Rio 2016, you can write:

```
olympics
  .'filter data'.'Games is'.'Rio (2016)'.then
  .'group data'.'by Athlete'.'sum Gold'.then
  .'sort data'.'by Gold descending'.then
  .'paging'.take(8)
  .'get the data'
```

Rich tooling is available when writing code using The Gamma web-based editor and so you get
auto-completion for available operations when typing `.`, you can see a live preview of 
the transformed data and you can even modify the code using a simple user interface.

## Documentation

The full project documentation is available on [thegamma.net](http://thegamma.net) web site:
 
 - [Contributing: Building from the source code](http://thegamma.net/contributing/)
   explains how to build everything locally for those who want to contribute!
 - [Developers: Using The Gamma JavaScript library](http://thegamma.net/developers/) 
   discusses how to embed visualizations using The Gamma JavaScript library and how
   to create and interact with the built-in editor.
 - [Data: Providing data as a REST services](http://thegamma.net/publishing/)
   discusses how to provide data for the visualization. This is just a matter of
   writing a simple service that evaluates queries.
 - [Exploring: Aggregating and visualizing data](http://thegamma.net/exploring/)
   covers The Gamma scripting language and the tools that you and your readers can
   use to explore and visualize data.

## Credits and license
The Gamma is built at [The Alan Turing Institute](https://www.turing.ac.uk/) and has been 
supported through the [Google Digitial News Initiative](https://www.digitalnewsinitiative.com/).
It is available as open-source using the permissive MIT license. This means that you are welcome
to contribute, modify the project as you wish and use it commercially without restrictions.
