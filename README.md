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

The Flutter app code lives in src/app/src/icare, and the Clojure backend lives in src/server/src (run it with `clojure -M:datamigo`). It serves both the negentropy sync protocol and push notifications. I think the app code makes the most sense when you start reading it from core.cljd. Here's a highish level diagram of the code's architecture.
<img width="1412" height="776" alt="Screenshot 2026-01-26 at 17 57 44" src="https://github.com/user-attachments/assets/7272a2a9-45c1-4023-8106-bb0a6a2a96c9" />
