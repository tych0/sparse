static void copy_to_user(void *to, const void *from, unsigned long n)
{
}

struct foo {
	int bar;
};

static void main(void)
{
	void *p;
	struct foo f;
	int uninitialized;

	copy_to_user(p, &f, sizeof(f));
	copy_to_user(p, &f, sizeof(f)-1);
	copy_to_user(p, &f, sizeof(f)+1);
	copy_to_user(p, &f, 1);
	copy_to_user(p, &f, 100);
	copy_to_user(p, &f, uninitialized);
}
/*
 * check-name: copy_to_user sizes
 *
 * check-error-start
copy_to_user_sizes.c:17:9: warning: copy_to_user() where size (5) is larger than src (4)
copy_to_user_sizes.c:19:9: warning: copy_to_user() where size (100) is larger than src (4)
 * check-error-end
 */
