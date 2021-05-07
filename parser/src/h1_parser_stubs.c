#include <string.h>
#include <caml/mlvalues.h>
#include <caml/bigarray.h>

// TODO: Remove this if https://github.com/inhabitedtype/bigstringaf/pulls gets merged
CAMLprim value
bigstringaf_memchr(value vba, value vba_off, value vchr, value vlen)
{
    size_t off = Unsigned_long_val(vba_off);
    char *buf = ((char *)Caml_ba_data_val(vba)) + off;
    size_t len = Unsigned_long_val(vlen);
    int c = Int_val(vchr);

    char* res = memchr(buf, c, len);
    if (res == NULL)
    {
        return Val_long(-1);
    }
    else
    {
        return Val_long(off + res - buf);
    }
}
