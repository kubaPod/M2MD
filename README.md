Consider it beta before it hits 1.0.0. It is undergoing major upgrades, as far as the time is found, in 2020 so let me know if anything is off. Before I write documentation release logs are shedding some light on syntax/options: https://github.com/kubaPod/M2MD/releases

# M2MD

Simple converter of Mathematica notebooks to markdown.

It originates from https://mathematica.stackexchange.com/q/84556/5478, it didn't evolve too much but I'm open to feedback as it is not something I use on daily basis.

## Installation
 
### Manual
 
   Go to 'releases' tab and download appropriate .paclet file.
    
   Run `PacletInstall @ path/to/the.paclet` file
   
### Via ``MPM` ``
   
If you don't have ``MPM` `` yet, run:
   
    Import["https://raw.githubusercontent.com/kubapod/mpm/master/install.m"]
   
and then:
   
    Needs @ "MPM`"    
    MPM`MPMInstall["kubapod", "m2md"]
    
From now on there should be a M2MD item in your Palettes menu.     
