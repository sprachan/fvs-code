## Setting up FVS and rFVS on Mac
October 2, 2025

These are the commands I ran in Terminal on Macbook Pro (2022, M2) to get FVS to build.

### Update homebrew and install needed packages:
```brew update 
brew upgrade 
brew install gcc 
brew install cmake 
brew install unixodbc
```

Where `gcc` gives me fortran, C, and C++ compilers; `cmake` gives me the tools to build the variants. I'm not sure what `unixodbc` does, but it was in the FVS instructions.

### Get actual volume equations
The NVEL directory I pulled when I pulled the FVS code was empty because it was linked to a different repo. Here, I link the volume library repo as a submodule using Git: 

```cd ~/Documents/ForestVegetationSimulator 
cd ./volume 
rm -rf NVEL 
git submodule add https://github.com/FMSC-Measurements/VolumeLibrary 
mv VolumeLibrary NVEL
```

Where I first clear out the existing (not helpful) NVEL directory, link the submodule, then rename it to match the CMake file. 

### Fix CMakeList.txt
I had some version number issues with Cmake (the minimum version number in the file in the Git repo is 2.8, but my version of CMake did not want to work with anything with a min version number less than 3.5), so I did a hacky fix where I went into the CMakeList file and changed the minimum version number.

```cd ../bin
vim CMakeList.txt
```

Then I changed lines 8 and 75 to read:
`cmake_minimum_required (VERSION 2.8...4.1)`
And saved these changes.

### Build FVS with CMake
Finally, run
`cmake -S . -G"Unix Makefiles"`

Which ran verbosely but without errors.
