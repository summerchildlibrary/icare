# iCare

iCare is not a lame social media.

To run this project you must first install clojuredart.

With clojuredart installed:
1. clone this repo
2. cd into this repo
3. run ```clojure -M:cljd init```

To compile and run after installing:
1. open a device emulator or setup flutter to run on a physical device
2. Run ```flutter devices``` to get a list of all your available devices
3. Run ```clojure -M:cljd flutter -d <device id from previous step>```

More documentation for running a clojuredart project can be found in the clojuredart github repo [here.](https://github.com/Tensegritics/ClojureDart)

The code for this project lives in src/icare. I think the code makes the most sense when you start reading it from core.cljd. Here's a highish level diagram of the code's architecture.<img width="1704" height="1454" alt="icare_architecture" src="https://github.com/user-attachments/assets/7733d01d-68ff-4fdc-8265-60f059b6c86e" />
