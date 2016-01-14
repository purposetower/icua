#include <stdio.h>
#include <unistd.h>
#include <termios.h>
#include <stdbool.h>
#include <sys/select.h>

int main() {
    printf("Starting\n");

    struct termios raw;
    
     /* input modes - clear indicated ones giving: no break, no CR to NL, 
       no parity check, no strip char, no start/stop output (sic) control */
    raw.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);

    /* output modes - clear giving: no post processing such as NL to CR+NL */
    raw.c_oflag &= ~(OPOST);

    /* control modes - set 8 bit chars */
    raw.c_cflag |= (CS8);

    /* local modes - clear giving: echoing off, canonical off (no erase with 
       backspace, ^U,...),  no extended functions, no signal chars (^Z,^C) */
    raw.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);

    /* control chars - set return condition: min number of bytes and timer */
    raw.c_cc[VMIN] = 5; raw.c_cc[VTIME] = 8; /* after 5 bytes or .8 seconds
                                                after first byte seen      */
    raw.c_cc[VMIN] = 0; raw.c_cc[VTIME] = 0; /* immediate - anything       */
    raw.c_cc[VMIN] = 2; raw.c_cc[VTIME] = 0; /* after two bytes, no timer  */
    raw.c_cc[VMIN] = 0; raw.c_cc[VTIME] = 8; /* after a byte or .8 seconds */
    
    tcsetattr(STDOUT_FILENO, TCSAFLUSH, &raw);

    int ready;
    int c;
    char c_tty;
    fd_set rdfs;

    while(true) {

        FD_ZERO(&rdfs);
        FD_SET(STDIN_FILENO, &rdfs);

        /* Block until input becomes available or timeout */
        ready = select(STDIN_FILENO + 1, &rdfs, NULL, NULL, NULL);
        if (ready) {
            c = read(STDIN_FILENO, &c_tty, 3);
            write(STDOUT_FILENO, &c_tty, 3);
            putchar('k');
            //putchar(c_tty);
            fflush(stdout);
        }
    }
   
    return(0);
}
