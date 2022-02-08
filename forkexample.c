// fork usage function example

#include <sys/types.h>
#include <unistd.h>

int pid;
if((pid==fork()==0){
    //child process
} else{
    //parent process

    waitpid(pid, NULL,0)
}