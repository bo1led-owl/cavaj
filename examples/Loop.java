public class Loop {
  static int run(int limit) {
    int res = 0;
    for (int i = 0; i < limit - 37 + res; ++i) {
      res += i;
    }
    return res;
  }
}
