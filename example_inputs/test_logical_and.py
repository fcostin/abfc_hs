DEF_MACRO('main')(
    LOCAL('t1'),
    LOCAL('t2'),
    LOCAL('f1'),
    LOCAL('f2'),
    LOCAL('r'),
    LOCAL('not_r'),
    CLEAR('t1'),
    CLEAR('t2'),
    CLEAR('f1'),
    CLEAR('f2'),
    CLEAR('r'),
    CLEAR('not_r'),
    CONSTANT_ADD(INT_CONSTANT(1), 't1'),
    CONSTANT_ADD(INT_CONSTANT(1), 't2'),
    LOGICAL_AND('f1', 'f2', 'r'),
    LOGICAL_NOT('r', 'not_r'),
    IF('r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{1} f1 f2 fail\n')),
    ),
    IF('not_r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{1} f1 f2 pass\n')),
    ),
    LOGICAL_AND('t1', 't2', 'r'),
    LOGICAL_NOT('r', 'not_r'),
    IF('r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{2} t1 t2 pass\n')),
    ),
    IF('not_r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{2} t1 t2 fail\n')),
    ),
    LOGICAL_AND('t1', 'f2', 'r'),
    LOGICAL_NOT('r', 'not_r'),
    IF('r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{3} t1 f2 fail\n')),
    ),
    IF('not_r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{3} t1 f2 pass\n')),
    ),
    LOGICAL_AND('f1', 't2', 'r'),
    LOGICAL_NOT('r', 'not_r'),
    IF('r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{4} f1 t2 fail\n')),
    ),
    IF('not_r')(
        PUT_STRING_CONSTANT(STRING_CONSTANT('{4} f1 t2 pass\n')),
    ),
)
