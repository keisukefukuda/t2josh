9deg - A job submittion helper for TSUBAME2.0
==============================================

Japanese version is [here](https://github.com/keisukefukuda/9deg/blob/develop/README.ja.md)

This program is provided "as is", without any warranty.
Please read LICENSE file for defails.

This is a 'third-party' program. Please do NOT contact Tokyo Tech GSIC
support desk about this program.
Please send feature requests or bug reports to twitter/@keisukefukuda or
keisukefukuda_at_gmail.com.

Introduction
------------

9deg is a job submission helper script for TSUBAME 2.0.
To submit a job on TSUBAME2.0, you have to (1) prepare a job script which includes
environmental variable settings, and (2) call 't2sub' command with correct arguments,
such as #node/#procs/#cups.

'9deg' does these chores for you, which include

 - preparing a shell script to be executed by t2sub
 - calculate # of processes/nodes/ncpus and set OpneMP evn vars
 - detect compilation environment(compiler and MPI library choice)

You can see what generated shell scripts and commands look like by using --dry option, which means 'dry run'.

Examples
--------

Let's see some examples:

### Example: Invoke a serial program

    $ 9deg -g t2g-yourgroup --dry -- ./a.out -n 1 2 3

produces the below

     #!/bin/sh
     #----------------------------------------------
     # Generated by 9deg at 2011-11-07 18:44:24.693997
     # in directory /home0/usr1/11M37264/github/9deg
     # t2sub -q S -W group_list=t2g-yourgroup -l select=1 /home/usr1/11M37264/.9deg/111107184424_650.sh
     # /home/usr1/11M37264/.9deg/111107184424_650.sh
     cd ${PBS_O_WORKDIR}
     
     export PATH=/usr/apps/openmpi/1.4.2/intel/bin:$PATH
     export LD_LIBRARY_PATH=/usr/apps/openmpi/1.4.2/intel/lib:$LD_LIBRARY_PATH
     
     ./a.out -n 1 2 3
     
     #----------------------------------------------
     
     


You can see the generated shell script and command line.

Text between "#----" lines is content of the generated temporary shell script,
which includes generated t2sub command line as a comment.

'--' is used to separate arguments for 9deg and program to be executed
with its arguments. You can also write like
     $ 9deg -g t2g-yourgroup --dry "./a.out -n 1 2 3"
without "--". Since we used --dry option here, nothing is executed and we can observed
what 9deg command was trying to do.

"-g" option is used to specify your TSUBAME group. There is a warning
that no such group as 't2g-yourgroup', which we used as a dummy group name.

S queue is used by default. You can change queue by using -q option.


### Example: Invoke an MPI program on 4 nodes, with 8 processes per node.

     $ ./9deg -n 8x4 -g t2g-yourgroup -q S --dry ./a.out

produces:

     #!/bin/sh
     #----------------------------------------------
     # Generated by 9deg at 2011-11-07 18:44:24.790873
     # in directory /home0/usr1/11M37264/github/9deg
     # t2sub -q S -W group_list=t2g-yourgroup -l select=4:mpiprocs=8:ncpus=8 /home/usr1/11M37264/.9deg/111107184424_150.sh
     # /home/usr1/11M37264/.9deg/111107184424_150.sh
     cd ${PBS_O_WORKDIR}
     
     export PATH=/usr/apps/openmpi/1.4.2/intel/bin:$PATH
     export LD_LIBRARY_PATH=/usr/apps/openmpi/1.4.2/intel/lib:$LD_LIBRARY_PATH
     
     mpirun -n 32 -hostfile ${PBS_NODEFILE} ./a.out
     
     #----------------------------------------------
     
     

### Example: Invoke non-MPI OpenMP program

     $ ./9deg --openmp=4 -g t2g-yourgroup -q S --dry ./a.out

produces:

     #!/bin/sh
     #----------------------------------------------
     # Generated by 9deg at 2011-11-07 18:44:24.859738
     # in directory /home0/usr1/11M37264/github/9deg
     # t2sub -q S -W group_list=t2g-yourgroup -l select=1:ncpus=4 /home/usr1/11M37264/.9deg/111107184424_651.sh
     # /home/usr1/11M37264/.9deg/111107184424_651.sh
     cd ${PBS_O_WORKDIR}
     
     export OMP_NUM_THREADS=4
     
     export PATH=/usr/apps/openmpi/1.4.2/intel/bin:$PATH
     export LD_LIBRARY_PATH=/usr/apps/openmpi/1.4.2/intel/lib:$LD_LIBRARY_PATH
     
     ./a.out
     
     #----------------------------------------------
     
     

NOTE: -n options implies MPI, so if you want to run a non-MPI OpenMP program, you have to use '--openmp=N'.

### Example: an MPI & OpenMP hybrid program

     $ ./9deg -n 4x4x2 -g t2g-yourgroup -q S --dry ./a.out

produces:

     #!/bin/sh
     #----------------------------------------------
     # Generated by 9deg at 2011-11-07 18:44:24.929626
     # in directory /home0/usr1/11M37264/github/9deg
     # t2sub -q S -W group_list=t2g-yourgroup -l select=2:mpiprocs=4:ncpus=16 /home/usr1/11M37264/.9deg/111107184424_151.sh
     # /home/usr1/11M37264/.9deg/111107184424_151.sh
     cd ${PBS_O_WORKDIR}
     
     export OMP_NUM_THREADS=4
     
     export PATH=/usr/apps/openmpi/1.4.2/intel/bin:$PATH
     export LD_LIBRARY_PATH=/usr/apps/openmpi/1.4.2/intel/lib:$LD_LIBRARY_PATH
     
     mpirun -n 8 -hostfile ${PBS_NODEFILE} ./a.out
     
     #----------------------------------------------
     
     
