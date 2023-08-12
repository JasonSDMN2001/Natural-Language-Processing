
s(s(NP,VP))-->np(NP),vp(VP).
np(np(D, N))-->det(D),noun(N).
vp(vp(V,NP,PP))-->verb(V),np(NP),pp(PP).
pp(pp(PR,NP))-->prep(PR),np(NP).
noun(noun(waiter))-->[waiter].
noun(noun(meal))-->[meal].
noun(noun(table))-->[table].
verb(verb(brought))-->[brought].
det(det(the))-->[the].
prep(prep(to))-->[to].



