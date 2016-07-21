# SSAC-2017-
Repository for code and notes, try to keep data files on personal computer. 

## Getting started
There is a starter file in the `/data` folder of this repo to get started. Beware of doing things like trying to print the entire file to the console at once and expecting to actually see it all, or the like; it is 99.9MB.

## How to access SportVU data
There is a GitHub repository with a lot of basketball data [here](https://github.com/neilmj/BasketballData/tree/master/2016.NBA.Raw.SportVU.Game.Logs). As far as I know the only way to download the relevant data is to download all of it. About half of the data there is the relevant SportVU data, as I recall, and the other half can be discarded.

The data comes in a zip file with extension 7z, which stands for [7zip](http://www.7-zip.org/). It looks like Scott was right about it being built for Windows, but it can also be used from the command line from Macs, apparently. Here is one such [link](http://7zx.updatestar.com/) (untried by me).

I remember having difficulty figuring out what to run in the command line for the next step. Once it worked, though, a single command unzipped every file and stored it in the destination folder I specified. When I re-figure that out I will update this README.