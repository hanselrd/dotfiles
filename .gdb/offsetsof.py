import gdb
import textwrap

class Offsets(gdb.Command):
    def __init__(self):
        super(Offsets, self).__init__('offsetsof', gdb.COMMAND_DATA)

    def _offsets(self, stype, sfield=None, indent=0):
        last_field = None
        padding = 0

        prefix = ''
        code = stype.strip_typedefs().code
        if code is gdb.TYPE_CODE_STRUCT:
            prefix = 'struct'
        elif code is gdb.TYPE_CODE_UNION:
            prefix = 'union'
        else:
            prefix = '<unknown>'

        stype_name = ''
        if stype.name is None:
            stype_name = '<anonymous>'
        else:
            stype_name = stype.name
        print(textwrap.indent('{} {} [{}] {{'.format(prefix, stype_name, stype.sizeof), ' ' * 4 * indent))

        for field in stype.fields():
            # padding bytes were added
            if last_field != None and last_field.bitsize == 0 and ((last_field.bitpos / 8) + last_field.type.sizeof) != (field.bitpos / 8):
                tmp1 = int(last_field.bitpos / 8) + last_field.type.sizeof
                tmp2 = int(field.bitpos / 8) - tmp1
                # unions cause a padding false-positive
                if tmp2 > 0:
                    print(textwrap.indent('__pad__; => {} [{} padding]'.format(tmp1, tmp2), ' ' * 4 * (indent + 1)))
                    padding += tmp2
            code = field.type.strip_typedefs().code
            if code is gdb.TYPE_CODE_STRUCT or code is gdb.TYPE_CODE_UNION:
                self._offsets(field.type, field, indent + 1)
            else:
                tmp3 = int(field.bitpos / 8)
                print(textwrap.indent('{} {}; => {} [{}]'.format(field.type.name, field.name, tmp3, field.type.sizeof), ' ' * 4 * (indent + 1)))
            last_field = field

        if last_field != None and last_field.bitsize == 0 and ((last_field.bitpos / 8) + last_field.type.sizeof) != stype.sizeof:
            tmp4 = int(last_field.bitpos / 8) + last_field.type.sizeof
            tmp5 = stype.sizeof - tmp4
            print(textwrap.indent('__pad__; => {} [{} padding]'.format(tmp4, tmp5), ' ' * 4 * (indent + 1)))
            padding += tmp5

        if sfield is None:
            print(textwrap.indent('}}; [{}] ({} padding)'.format(stype.sizeof, padding), ' ' * 4 * indent))
        else:
            tmp6 = int(sfield.bitpos / 8)
            print(textwrap.indent('}} {}; => {} [{}]'.format(sfield.name, tmp6, stype.sizeof), ' ' * 4 * indent))

    def invoke(self, arg, from_tty):
        argv = gdb.string_to_argv(arg)
        if len(argv) != 1:
            raise gdb.GdbError('offsetsof takes only 1 argument.')

        self._offsets(gdb.lookup_type(argv[0]))

Offsets()
