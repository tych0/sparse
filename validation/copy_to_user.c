#define __user __attribute__((address_space(1)))

struct bar {
	char *bar_kptr;
};

struct foo {
	char *foo_kptr;
	char __user *uptr;
	struct bar bar;
};

static void copy_to_user(void __user *to, const void *from, unsigned long n)
{
}

static void bar(void)
{
	struct foo f;
	void __user *p = (void __user *)0;

	copy_to_user(p, &f, sizeof(f));
}
/*
 * check-name: copy_to_user arguments
 *
 * check-error-start
copy_to_user.c:22:9: warning: member foo_kptr is a kernel pointer copied to userspace
copy_to_user.c:22:9: warning: member bar_kptr is a kernel pointer copied to userspace
 * check-error-end
 */
