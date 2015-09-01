# (Insert name here)

This software is made to assist your brain after a roleplay session.
It is designed to be kind of an archive of what happened in your worlds. This is, a history which will grow over time, and a number of wiki like sites for all additional information beside the history.

## What it needs to run

But first, there is some downside (not really in my opinion): The server is _not_ written in PHP or any other scripting language, so you probably have no chance to let it run on of the standard hosting services.
Instead, the server is written in Ada, with big support of the [Ada Web Server](http://libre.adacore.com/tools/aws/) (this is some great piece of software! Written in a great piece of language [sic!]). The database part is done via [PostgreSQL](http://www.postgresql.org), and on the client side, I use the [Dojo Toolkit](http://dojotoolkit.org), also not that common, but the core also is great.
This means in conclusion: You need some server, where you can run executables on. Also, your platform choice should not be too exotic, as you have to compile the Ada stuff, but with [Debian](https://www.debian.org) you get great support. And you need some tool to convert the images, but this comes later.

## What it can

### Users, gamemaster, characters

We have many groups, some with more than one gamemaster, and each user playing in multiple groups, but not in all. So, a user can own groups, where he then has the role of a gamemaster. Also, a user can be character. Other than a gamemaster, a user can have multiple characters per group. Also, one can have a character in a group, one owns itself.

### The differenct sections

Per group, a character has his own page, where he can write down stuff he wants to archive, like his history and so on. Also, he can create galleries attached to his character, each containing a bunch of pictures.
There also is a group wide gallery section, an archive for maps, drawings and so on.
The group's history is managed by the group's gamemasters, which can create new entries and manipulate them.
In the wiki, we have a just a set of pages with tags and information. History and wiki are meant as follows:
In the history, one sentence could be: "Karl searched for the flower of Nerull, to pay his debts to the king.". Now, where to put the information about this flower, and the king? It would get lost in the history as we after the next five entries, we cannot remember where we put it. So we create a wiki page for it.
Basically, this is all.

### Textmarkup and Privacy

The text is rendered by a slightly modified version of [marked](https://github.com/chjj/marked).
As sometimes, a gamemaster or a player want to keep things private, there is a possibility to hide information from other players (but never from the gamemaster) - either as text annotations, or in a bigger way, as permissions per history entry or wiki site.

### Cool stuff (in my opinion)

As the history grows arbitrarily long, it will be loaded if needed. This is mostly true for everything. It behaves mostly like a one page application, so there are no page refreshes necessary. Websockets and Ajax do the communication and synchronization part. So, in theory, if you found a situation where you have to reload, then you found a bug. I hope, that this will hold in the future, otherwise there will be a section with exceptions to this rule.
The synchronization also works over devices, so if you log into you account via laptop and mobile phone, you receive all updates. And logout will log you out on every device (which is more a security consideration).
And well, Ada runs on the server, this is great enough :)
Also, the logic is split up - The client renders everything, the server distributes data, but nearly does no logic concerning the data. This is all done in the database via a bunch of stored procedures, as the data resides there. And the server calls them via prepared statements and ensures,that every incoming request is valid via Ada's type system.

### Some concerns

This website is created to use in private. There yet is no interface for a public planned, if you want to share your roleplay stuff.
But this also means, that I assume you trust your users. Because of that, there is no validation of input. This site is absolutely _NOT_ safe against XSS attacks. I have never tested it, and never developed actively against it.
But as trust is best established with control, there is a permission system, with the intend to restrict a character's access to some ressources, if a GM or character want to.

## Installation

### Prerequisites

I recommend you to do it with debian, as they have packages for the ada web server and gnat json library. But as I first started development on Arch Linux, I can guarantee, that it is also possible to get it up and running there (but really, I have no idea, how I did it).

* You do not really need the "make" tool, but you can use it.
* The JSON library from the gnatcoll library.
* Ada Web Server in version 3.2.0
* imagemagick (or some other tool which uses /usr/bin/convert). But you can also modify the according file yourself (image_handling.adb). If you can't (like in "I really have _no_ idea"), drop a mail.
* The Dojo Toolkit, v1.10. Simply put the dojo directory into static/js/.
* For encryption, OpenSSL is required. Create a keypair, and name it "aws-server.key" and "aws-server.cert" (or change server.adb). Put them into the directory of the server executable.
* You need PostgreSQL. Better is always a new version, I use 9.4. Prepare a database user to run with, and set up all required configurations. And you need the pgcrypto extension.  
  The database script will create a "rpg" database. If you want another name, you have to manually search & replace it in every sql/\*.sql file. Also, change the connectionstring in the server.adb to your needs.
* Create a .pgpass file in the home directory of the user, who runs the server. You find the information about this in the web.
* You can change the port in aws.ini.
* How much debugging information do you want to print? Probably, nothing at all. So, comment out the "AWS.Server.Log..." lines from server.adb, and change the Log_Level in util.ads to None.
* Some thing to consider: Dump the database sometimes! In terms of testing, I have been best example for how _not_ to do it right. Testing was most times destructive, simple trying if it works. So, there is the small script rpgbkp, which you could put into a cronjob to run sometimes. (Also, I am no god, so I won't say something like "it should be save" - simply, do backups and tell me if something is weird).

Not that much at all :) And you could put your eyes on some Ada code, isn't that great?

### Install

* Run "make", or run the command in the "all:" section of makefile.
* Run the sql/database.sql script to create all tables and so on.
* Start the server with ./server  
  There is one thing, I cannot explain, but somehow I didn't get it running with systemd, so I currently always use GNU screen and detach it.

After you did this, you can run the shit, via executing "./server" on the command line. You now should be able to connect to it via your browser (https://<ip-address>:<Port>). Can you see it? I hope so! But I haven't tried it.

## How to use

Therefor I created some videos.
_But_ one thing probably is a big pain in the ass: Activating users. I don't really like the idea of publishing a user activation api to the public, so there is none. If a user registrates, you have to log into you postgres account and execute two commands (if you know a better solution, tell me, but I think, this is not too hard):
```SQL
select id, name from registered;
```
There, you have to look for the id of the user's name you want to activate. Then, do 
```
update registered set pending=false where id=12345;
```
But, you should exchange the id with the id you found.

## Plans for the future

* Minify the JavaScript sources. They are far too big.
* Clean the JavaScript files. The naming scheme should be made consistent, and also the structure inside the packages.
* It would be cool to have a site with an overview over all permissions.
* Understand, how to extend marked correctly, to add more features to the textlayout (easier private regions, and maybe some other stuff.)
* Test everything ... :D
* React to your ideas!
* Error / notification messages are not good yet.
  * They should be printed better to the user
  * Or printed at all, if necessary.
* i18n would be nice ...
