# RISC-V Bluespec

## Getting Started

### Environment Setup
You need to source the provided setup script scripts/envsetup.sh

```
source scripts/envsetup.sh
```

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
```
This will install 

## Running Linux

### Building the RISC-V GNU Linux cross-compiler

```
cd vendor/riscv-gnu-toolchain
./configure --disable-float --disable-atomic --with-arch=IM --prefix=$RISCV_TOOLS
make linux
```

