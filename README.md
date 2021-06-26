amoveo_p2p_derivatives_explorer
=====

The purpose of this tool is so that people can make P2P derivatives offers from the amoveo light node, and post them to this website. The website organizes all the different trade offers that are posted.


turn it on, then open this in a browser: http://localhost:8090/main.html


Turning it on and off
=======

First make sure you have an Amoveo node running and fully synced, and that the keys are unlocked on that node.

If you need to resync your full node, be sure to turn off this p2p_derivatives tool first.

```
sh start.sh
```

To connect to it, so you can give it commands:
```
sh attach.sh
```
If it says "Node is not running!", this means that the Amoveo p2p derivatives explorer is shut off and there is nothing for you to connect to. So try using start.sh to turn it on.

To disconnect, and allow it to run in the background, hold the CTRL key, and press D.

Then to turn it off, make sure you are attached, and run:

```
utils:off().
```
then run:
```
halt().
```

Syncing smart contracts
============
```
scalar_contracts:sync({159,89,87,58}, 8090).
buy_veo_orders:sync({159,89,87,58}, 8090).
```

this will pull all the oracle text, and what you need to settle the scalar contracts.