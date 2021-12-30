# 前言

探索程序的编译和执行

#### 初代

1. 编写c程序
2. 使用不同汇编器编译c程序来生成不用的二进制文件
3. 开发处理器核
4. 研究不同汇编器编译的程序以及cpu使用不同方法实现的指令对程序执行的影响

#### 二代

1. 设计汇编器
2. 自定义指令架构
3. 编译更加复杂的源程序
4. 比较执行速率

# 启程

## Environment

1. A machine---Used to upstall ubuntu
2. A ubuntu(20.04) operating system---Used to upstall Google
3. A google browser---Used to solve all problems

Find a directory that you like, then build your project directory.

```shell
mkdir paradise
cd paradise
```

```shell
echo export NOOP_HOME=$(pwd) >> ~/.bashrc
echo export AM_HOME=$(pwd)/abstract-machine >> ~/.bashrc
echo export NEMU_HOME=$(pwd)/NEMU >> ~/.bashrc
echo export DRAMSIM3_HOME=$(pwd)/DRAMsim3 >> ~/.bashrc
source ~/.bashrc

```

## Configuration

```shell
cd $NEMU_HOME
make menuconfig
make riscv64-xs-ref_defconfig
make -j4

```

```shell
cd $DRAMSIM3_HOME
mkdir build
cd build
cmake -D COSIM=1 ..
make -j4
cmake .. -DTHERMAL=1

```

## Cross-compiler

After configuring the environment above, you will have some cross-compilers on your machine:
1. mips32-nemu
2. native
3. riscv32-nemu
4. riscv64-mycpu
5. riscv64-nemu
6. spike
7. x86_64-qemu
8. x86-nemu
9. x86-qemu

#### Use it to compile source files

A short of tests are provided in the am-kernels directory, starting your Compile_execute journey.

Such as:

```shell
cd $NOOP_HOME
cd am-kernels/tests/cpu-tests
make ARCH=riscv64-mycpu ALL=add
```

ARCH=the cross-compiler you want to use, ALL=the test name you want to compile

Only use ARCH argument will compile all of tests in pwd.

Then, you will find the binary file in the build directory.

## Run tests

After completing all of above step by step, you can run the test with your own CPU.

Such as:

```shell
cd $NOOP_HOME
make emu
./build/emu -i am-kernels/tests/cpu-tests/build/riscv64-mycpu/add-riscv64-mycpu.bin
```

Indicates that the test has passed after a `HIT GOOD TRAP` is displayed on the terminal.

![image](https://github.com/yuhanzhu612/Compile_execute/blob/main/HIT%20GOOD%20TRAP.png)

If your CPU is not right, you can see `ABORT at pc` or `HIT BAD TRAP`.

Use Difftest to debug your CPU.

## Resources

[abstract-machine](https://github.com/NJU-ProjectN/abstract-machine)

[difftest](https://github.com/OpenXiangShan/difftest)

[am-kernels](https://github.com/NJU-ProjectN/am-kernels)

[NEMU](https://github.com/OpenXiangShan/NEMU)

[DRAMsim3](https://github.com/umd-memsys/DRAMsim3)

[Trigger](https://github.com/yuhanzhu612/Trigger)


