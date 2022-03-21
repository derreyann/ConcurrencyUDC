#include <errno.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>
#include "options.h"


#define MAX_AMOUNT 20

int max(int num1, int num2)
{
    return (num1 > num2 ) ? num1 : num2;
}
int min(int num1, int num2) 
{
    return (num1 > num2 ) ? num2 : num1;
}

struct bank {
    int num_accounts;        // number of accounts
    int *accounts;           // balance array
    pthread_mutex_t *mutex; //added mutexes to the bank accounts structure for easy passthrough to functions
};

struct args {
    int          thread_num;  // application defined thread #
    int          delay;       // delay between operations
    int	         iterations;  // number of operations
    int          net_total;   // total amount deposited by this thread
    int          done;   
    pthread_mutex_t *mutex;
    struct bank *bank;        // pointer to the bank (shared with other threads)
};

struct thread_info {
    pthread_t    id;    // id returned by pthread_create()
    struct args *args;// pointer to the arguments
};
/**
 * @brief the transfer function, utilizes mutex1 and mutex2?
 * generates two different accounts to transfer between, the amount transfered in the interval [1;balance of the account]
 * @param ptr arguments passthrough
 * @return void* 
 */
void *transfers(void *ptr){
    struct args *args =  ptr;
    int amount;
    int account1, account2;
    while(args->iterations--){
        account1 = rand()%args->bank->num_accounts;
        account2 = rand()%args->bank->num_accounts;
        while(account2==account1 /*&& args->bank->accounts[account1]<args->bank->accounts[account2]*/){
            account2 = rand()%args->bank->num_accounts;
        }
        pthread_mutex_lock(&args->bank->mutex[min(account1, account2)]);
        pthread_mutex_lock(&args->bank->mutex[max(account1, account2)]);

        amount = rand()%args->bank->accounts[account1];
        //End of prep step
        printf("\033[0;32m");
        printf("Thread %d Randomly Transfering %d from %d to %d\n", args->thread_num, amount, account1, account2);
        if(args->delay) usleep(args->delay);
        printf("\033[0;37m");
        args->bank->accounts[account1]-=amount;
       // pthread_mutex_lock(&args->bank->mutexacc1[account2]);
        args->bank->accounts[account2]+=amount;
        usleep(1);
        if(args->delay) usleep(args->delay);
        pthread_mutex_unlock(&args->bank->mutex[min(account1, account2)]);
        pthread_mutex_unlock(&args->bank->mutex[max(account1, account2)]);

    }
    return NULL;
}

int check(struct args *args){
    pthread_mutex_lock(&args->mutex);
    int done = args->done;
    pthread_mutex_unlock(&args->mutex);
    return done;
}

/**
 * @brief amount function, running on the extra thread (by default 6th thread)
 * gives continuous total balance during deposits, is killed at the end of deposits
 * @param ptr 
 * @return void* 
 */
void *amount(void *ptr){
    struct args *args =ptr;
    while(check(args)==0){
        //usleep(1000);
        int bank_total=0;
        for(int i=0; i < args->bank->num_accounts; i++) {
            usleep(1);
            pthread_mutex_lock(&args->bank->mutex[i]);
        }
        for(int i=0; i < args->bank->num_accounts; i++) {
            bank_total += args->bank->accounts[i];
        }
        for(int i=0; i < args->bank->num_accounts; i++) {
            pthread_mutex_unlock(&args->bank->mutex[i]);
        }
        //if(args->delay) usleep(1500);
        printf("Total: %d\n", bank_total);
    }
    return NULL;
}

// Threads run on this function
void *deposit(void *ptr)
{
    struct args *args =  ptr;
    int amount, account, balance;
    while(args->iterations--) {
        amount  = rand() % MAX_AMOUNT;
        account = rand() % args->bank->num_accounts;

        printf("Thread %d depositing %d on account %d\n",
               args->thread_num, amount, account);
        pthread_mutex_lock(&args->bank->mutex[account]);
        balance = args->bank->accounts[account];
        if(args->delay) usleep(args->delay); // Force a context switch

        balance += amount;
        if(args->delay) usleep(args->delay);

        args->bank->accounts[account] = balance;
        if(args->delay) usleep(args->delay);
        pthread_mutex_unlock(&args->bank->mutex[account]);
        args->net_total += amount;
    }

    return NULL;
}

// start opt.num_threads threads running on deposit.
struct thread_info *start_threads(struct options opt, struct bank *bank)
{
    int i;
    struct thread_info *threads;

    printf("creating %d threads\n", opt.num_threads);
    threads = malloc(sizeof(struct thread_info) * (opt.num_threads+1)); //exercice4, used num_thread+1 for dynamic alloc to avoid changing variables
    if (threads == NULL) {
        printf("Not enough memory\n");
        exit(1);
    }

    // Create num_thread threads running swap()
    for (i = 0; i < opt.num_threads; i++) {
        threads[i].args = malloc(sizeof(struct args));

        threads[i].args->thread_num = i;
        threads[i].args->net_total = 0;
        threads[i].args->bank = bank;
        threads[i].args->delay = opt.delay;
        threads[i].args->done = 0;
        threads[i].args->iterations = opt.iterations/opt.num_threads; //exercice4, modified the value to be shared between the threads, ie 100 each in example

        if (0 != pthread_create(&threads[i].id, NULL, deposit, threads[i].args)) {
            printf("Could not create thread #%d", i);
            exit(1);
        }

    }
    //pthread_join(threads[5].id, NULL); //wait for end it
    for(i=0; i<=opt.num_threads;i++){
        pthread_join(threads[i].id, NULL); //clears the 5 threads for repurpose
    }
    for(i=0; i<opt.num_threads;i++){
        threads[i].args->iterations = opt.iterations/opt.num_threads; //assigns new work to thread, starts transfers
        pthread_create(&threads[i].id, NULL, transfers, threads[i].args);
    }
    pthread_create(&threads[opt.num_threads].id, NULL, amount, threads[opt.num_threads-1].args); //exercice 3, calls on the last thread for balance update (default, 6th)
    for(i=0; i<opt.num_threads;i++){
        pthread_join(threads[i].id, NULL); //clears the 5 threads for repurpose
    }
    threads[opt.num_threads-1].args->mutex = malloc(bank->num_accounts*sizeof(pthread_mutex_t));
    pthread_mutex_init(threads[opt.num_threads-1].args->mutex, NULL);
    pthread_mutex_lock(threads[opt.num_threads-1].args->mutex);
    threads[opt.num_threads-1].args->done=1;
    pthread_mutex_unlock(threads[opt.num_threads-1].args->mutex);
    pthread_join(threads[opt.num_threads].id, NULL);
    
    return threads;
}

/// Print the final balances of accounts and threads
void print_balances(struct bank *bank, struct thread_info *thrs, int num_threads) {
    int total_deposits=0, bank_total=0;
    printf("\nNet deposits by thread\n");

    for(int i=0; i < num_threads; i++) {
        printf("%d: %d\n", i, thrs[i].args->net_total);
        total_deposits += thrs[i].args->net_total;
    }
    printf("Total: %d\n", total_deposits);

    printf("\nAccount balance\n");
    for(int i=0; i < bank->num_accounts; i++) {
        printf("%d: %d\n", i, bank->accounts[i]);
        bank_total += bank->accounts[i];
    }
    printf("Total: %d\n", bank_total);
}

// wait for all threads to finish, print totals, and free memory
void wait_t(struct options opt, struct bank *bank, struct thread_info *threads) {
    // Wait for the threads to finish
    for (int i = 0; i < opt.num_threads; i++)
        pthread_join(threads[i].id, NULL);

    print_balances(bank, threads, opt.num_threads);

    for (int i = 0; i < opt.num_threads; i++)
        free(threads[i].args);

    free(threads);
    free(bank->accounts);
}

// allocate memory, and set all accounts to 0
/**
 * @brief mutex declarations. 3 mutex utilized in program
 * 
 * @param bank 
 * @param num_accounts 
 */
void init_accounts(struct bank *bank, int num_accounts) {
    bank->num_accounts = num_accounts;
    bank->accounts     = malloc(bank->num_accounts * sizeof(int));
    bank->mutex = malloc(bank->num_accounts*sizeof(pthread_mutex_t));
    for(int i=0; i < bank->num_accounts; i++){
        bank->accounts[i] = 0;
        pthread_mutex_init(&bank->mutex[i], NULL);
    }
}

int main (int argc, char **argv)
{
    struct options      opt;
    struct bank         bank;
    struct thread_info *thrs;

    srand(time(NULL));

    // Default values for the options
    opt.num_threads  = 5;
    opt.num_accounts = 10;
    opt.iterations   = 100;
    opt.delay        = 10;

    read_options(argc, argv, &opt);

    init_accounts(&bank, opt.num_accounts);

    thrs = start_threads(opt, &bank);
    wait_t(opt, &bank, thrs);

    return 0;
}
