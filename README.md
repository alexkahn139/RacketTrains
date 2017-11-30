# RacketTrains
This is my repo for the programming project in the second year at the Vrije Universiteit Brussel.

It is based on the Belgian railwaysystem. There are two main parts NMBS, who is in charge to calculate the trajects and to display the user interface. The other one is Infrabel wich makes sure the trains drive and don't crash. Both communicate via TCP and can be run independently.

The project has a build in simulator but can also be used with the Z21 interface, to drive modeltrains.

To start the program you first have to start Infrabel, `racket -S .  InfrabelMain.rkt` to wich you can add the flag `-s` is you want to simulate the railway. After that you can start the NMBSmain.rkt file.

