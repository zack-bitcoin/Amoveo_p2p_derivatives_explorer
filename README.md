amoveo_p2p_derivatives_explorer
=====

This is a work in progress. It is not ready yet.

The purpose of this tool is so that people can make P2P derivatives offers from the amoveo light node, and post them to this website. The website organizes.

Todo
======
before this will be useable, we need to get these tasks done.
* background processes
* api
* javascript library
* UX


background processes
=========

scan and delete channel offers if they become invalid.

re-order oracles in terms of volume of available bets.

delete pointers to channel offers if they no longer exist.

if an oracle has no bet_offers in it, then remove it from oracles.erl


api
====

query the order oracle list

query oracle
* oid

query channel offer meta data
* list of cids

query channel offer text
* cid

submit trade offer
* channel_offer

javascript library
=========
write code to test all the different api calls.


UX
======

display the oracle question text in order of the volume of trades available in each market.
This data should be in pages so we don't load too much from the server at once.

allow the user to click on one of the oracles, and it loads the data from the oracle datastructure.
The channel offers are viewable in pages.

If they click on a channel offer, it should display the text they need to copy/paste to participate in the bet.


Turning it on and off
=======

First make sure you have an Amoveo node running, and that the keys are unlocked on that node.

```
sh start.sh
```

To connect to it, so you can give it commands:
```
sh attach.sh
```
If it says "Node is not running!", this means that the Amoveo mining pool is shut off and there is nothing for you to connect to. So try using start.sh again to turn it on.

To disconnect, and allow it to run in the background, hold the CTRL key, and press D.

Then to turn it off, make sure you are attached, and run:

```
utils:off().
```
then run:
```
halt().
```

