osu-share
=========
###share your maps to other osu! gamers.

Copyright (c) 2013, Tsukihime
All rights reserved.

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


##Instructions

- program configures the automatically.
- fine-tuning can be done by editing the file "osu!share.ini" (need restart).
- Sharing Maps functionality(through links on your local web server) requires your PC  would be able to receive incoming connections on the port 778(configured by default). Some information may be found here http://en.wikipedia.org/wiki/Port_forwarding

- "Puush map" functionality requires [puush](http://puush.me/) to be installed

- Copy link - copies beatmap link from local webserver to clipboard

###Features
- use right mouse click to show menu
![Interface](https://www.dropbox.com/s/55myb0em87oh7cv/Screenshot.png?dl=1)

- press Ctrl+M to insert clickable link to the current beatmap into chat.
![Using Ctl+M](https://www.dropbox.com/s/s70xxkoiavpsum4/CtrlMScreen.png?dl=1)

- Use Ctrl+mouse click on the map-link (eg https://osu.ppy.sh/b/799240) to search it in [bloodcat](http://bloodcat.com/osu/) (it works in game chat ie at a time when osu trying to open a browser).
![BloodCat Redirect](https://www.dropbox.com/s/ppt7wws3kjq92nk/bloodcat.png?dl=1)

##Troubleshooting

Some antivirus programs may panic when a program attempts to inject a hook module in the game, this is normal behavior, doubters can read the source code of the program.
