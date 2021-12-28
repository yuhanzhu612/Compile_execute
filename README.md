# Compile_execute

## 介绍

该目录是对程序编译和运行的探索

## 目的

### 初代

1. 编写c程序
2. 使用不同汇编器编译c程序来生成不用的二进制文件
3. 开发处理器核
4. 研究不同汇编器编译的程序以及cpu使用不同方法实现的指令对程序执行的影响

### 二代

1. 设计汇编器
2. 自定义指令架构
3. 编译更加复杂的源程序
4. 比较执行速率

## Environment

```shell
echo export NOOP_HOME=$(pwd) >> ~/.bashrc
echo export AM_HOME=$(pwd)/abstract-machine >> ~/.bashrc
echo export NEMU_HOME=$(pwd)/NEMU >> ~/.bashrc
echo export DRAMSIM3_HOME=$(pwd)/DRAMsim3 >> ~/.bashrc
source ~/.bashrc

```

## Config

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
cmake ..
make -j4
cmake .. -DTHERMAL=1

```
