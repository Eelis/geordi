
#include <assert.h>
#include <sys/ptrace.h>
#include <linux/ptrace.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <unistd.h>
#include <syscall.h>
#include <sys/reg.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __x86_64__
#define SYSCALL_OFF (ORIG_RAX * 8)
#else
#define SYSCALL_OFF (ORIG_EAX * 4)
#endif

void checked (char const * const s, int const r) { if (r == -1) { perror(s); abort(); } }

int main()
{
  pid_t const child = fork();

  checked("fork", child);

  if(child == 0)
  {
    checked("ptrace", ptrace(PTRACE_TRACEME, 0, NULL, NULL));
    sleep(2);
    checked("execl", execl("/usr/bin/whoami", "whoami", NULL));
  }

  checked("ptrace", ptrace(PTRACE_SETOPTIONS, child, NULL, PTRACE_O_TRACESYSGOOD));
    // Will fail saying: ptrace: No such process
    // Conclusion: disregarding sleep-solutions, this means the parent cannot reliably set PTRACE_O_TRACESYSGOOD before the first wait().

  return 0;
}
