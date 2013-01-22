#include <iostream>


struct Z {
  static int res(int i, ...) {
    return 0;
  }
};


struct N {
  static int res(int i) {
    return i + 1;
  }
};

template<int N>
struct U {
  
  template<typename... Args>
  static int res(int i, Args... args) {
    static_assert(N >= 0, "N should be non-negative");

    return U<N - 1>::res(args...);
  }

};

template<>
struct U<0> {

  template<typename... Args>
  static int res(int i, Args... args) {
    return i;
  }

};

template<typename F, typename... G>
struct S {
  
  template<typename... Args>
  static int res(Args... args) {
    return F::res(G::res(args...)...);
  }

};

template<typename F, typename G>
struct R {

  static int res(int x, int y) {
    return y == 0 ? F::res(x) : G::res(x, y - 1, res(x, y - 1));
  }

  static int res(int x, int x1, int y) {
    return y == 0 ? F::res(x, x1) : G::res(x, x1, y - 1, res(x, x1, y - 1));
  }
};

// Addition
typedef R<U<0>, S<N, U<2>>> Sum;

// Multiplication
typedef R<Z, S<Sum, U<2>, U<0>>> Mul;

// Subtraction
typedef R<Z, U<1>> DecImpl;
typedef S<DecImpl, Z, U<0>> Dec;
typedef R<U<0>, S<Dec, U<2>>> Sub;

// Comparsion
typedef R<Z, S<N, Z>> GZImpl;
typedef S<GZImpl, Z, U<0>> GZ;
typedef S<GZ, S<Sub, U<0>, U<1>>> GT;

typedef R<S<N, Z>, Z> NotImpl;
typedef S<NotImpl, Z, U<0>> Not;

typedef S<Not, S<GT, U<1>, U<0>>> GE;

// Integer division

typedef S<N, U<2>> IncZ;
typedef S<Mul, U<1>, IncZ> MulYZ;
typedef S<GE, U<0>, MulYZ> X_GE_YZ;
typedef S<Sum, U<3>, X_GE_YZ> DivRec;


typedef R<S<Z, U<0>>, DivRec> DivImpl;
typedef S<DivImpl, U<0>, U<1>, U<0>> Div;


typedef S<Sub, U<0>, S<Mul, S<Div, U<0>, U<1>>, U<1>>> Rem;

int sum(int x, int y) {
  return Sum::res(x, y);
}

int mul(int x, int y) {
  return Mul::res(x, y);
}

int sub(int x, int y) {
  return Sub::res(x, y);
}

int idiv(int x, int y) {
  return Div::res(x, y);
}

int rem(int x, int y) {
  return Rem::res(x, y);
}

int main() {
  
  using std::cout;
  using std::endl;

  //cout << IncZ::res(1, 2, 3) << endl;;
  //cout << MulYZ::res(1, 2, 3) << endl;
  //cout << X_GE_YZ::res(1, 2, 3) << " " << X_GE_YZ::res(10, 2, 3) << endl;
  //cout << DivRec::res(10, 2, 3, 2) << endl;
  
  cout << idiv(6, 3) << " " << idiv(4, 3) << endl;
  cout << rem(6, 3) << " " << rem(4, 3) << endl;
  return 0;
}
