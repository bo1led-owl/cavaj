public class Loop {
  static int run(int limit) {
    int res = 0;
    for (int i = 0; i < limit; ++i) {
      res += i;
    }
    return res;
  }
}
