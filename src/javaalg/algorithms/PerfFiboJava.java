package javaalg.algorithms;

import java.math.BigInteger;

public class PerfFiboJava {
    static public BigInteger fibs(int n) {
        BigInteger curr = BigInteger.valueOf(0);
        BigInteger next = BigInteger.valueOf(1);
        for (; n > 0; --n) {
            BigInteger nnext = curr.add(next);
            curr = next;
            next = nnext;
        }
        return curr;
    }
}
