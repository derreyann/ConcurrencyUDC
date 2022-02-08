#include <pthread.h>
//creating thread
/**
 * @brief Create a thread
 * 
 * @param thread 
 * @param attr 
 * @param start_fun 
 * @param arg 
 * @return int 
 */
int create_create(
    pthread *thread,        //thread id
    const pthread_attr_t *attr, //attributes
    void *(*start_fun)(void *), //start of thread
    void arg //argument passed thru
    ); //parameter of start function

//example of thread created
    thread(arg){
        //...
    }

/**
 * @brief waiting for thread creation
 * 
 * @param thread 
 * @param retval 
 * @return int 
 */
int pthread_join(
    pthread_t thread, //thread waiting for
    void **retval //return function
);

