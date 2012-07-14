Simple GUI validator to visualize strategies.


Dependencies:
------------

    gem install highline


How to run:
----------

    ./tools/gui-validator/validator.rb map16

You can susbtitute 'map16' for any other map that is available in tests directory.
You can also run it with an input of first moves by passing the -i option:

    ./tools/gui-validator/validator.rb map16 -i UR

Note: input the moves in the contest format of UDRLWA, not wasdop.


Automatic validator check:
-------------------------

You can run the validator in a non-interacive mode for automatic testing by
passing the -vv option, like this: 

    echo RDRRLLDDLLRRRRDDRDLULLDLLURRRRRUUR | ./tools/gui-validator/validator.rb -vv tests/contest3.map
    

Controls:
--------

* Moving - wasd
* Wait - o
* Abort - p

