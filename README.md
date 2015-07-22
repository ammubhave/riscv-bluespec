# RISC-V Bluespec

## Getting Started

### Environment Setup
You need to source the provided setup script scripts/envsetup.sh

```
source scripts/envsetup.sh
```
Remember to always source this environment setup script before doing any other tasks.
You will **also** need to set the `BLUESPECDIR` environment variable and include it in your path.

### Clone the git submodules
This will download all the external libraries and tools required to build.

```
git submodule init
git submodule update
```

### Building the RISC-V GNU Compiler Toolchain

```
cd vendor/riscv-gnu-toolchain
./configure --disable-float --disable-atomic --with-arch=IM --prefix=$RISCV_TOOLS
make
make linux
```
This will install the GNU compiler toolchain to `build/tools` directory.

### Building the RISC-V Frontend Server (fesvr) package

```
cd vendor/riscv-fesvr
mkdir build
cd build
../configure --prefix=$RISCV_TOOLS
make install
```

### Installing Connectal

You need to install Connectal tools and dependencies before you can compile and run any of the Bluespec files. Follow the installation instructions at `vendor/connectal/README.md` to get started.

## Running Linux

