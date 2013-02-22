#include <iostream>


struct Z {
  static int res(int i, ...) {
    return 0;
  }
};


struct N {
  static int res(int i, ...) {
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

int& last(int& k) {
	return k;
}

template<typename... Args>
int& last(int& k,  Args&... args) {
	return last(args...);
}

template<typename F, typename G>
struct R {
/*
  static int res(int x, int y) {
    return y == 0 ? F::res(x) : G::res(x, y - 1, res(x, y - 1));
  }

  static int res(int x, int x1, int y) {
    return y == 0 ? F::res(x, x1) : G::res(x, x1, y - 1, res(x, x1, y - 1));
  }*/
  template<typename... Args>
  static int res(Args... args) {
	//std::cout << "R::res\n";
	int& l = last(args...);
	//std::cout << l << "\n";
	if (l == 0) {
		//std::cout << "F\n";
		return F::res(args...);
	}
	//std::cout << "G\n";
	--l;
	return G::res(args...,  res(args...));
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

// Remainder
typedef S<Sub, U<0>, S<Mul, S<Div, U<0>, U<1>>, U<1>>> Rem;

// Power
typedef R<S<N, Z>, S<Mul, U<2>, U<0>>> Pow;

// Prime
typedef R<Z, S<Sum, U<2>, S<Not, S<Rem, U<0>, U<1>>>>> PrimeImpl;
typedef S<GE, S<N, Z>, S<PrimeImpl, U<0>, U<0>>> Prime;

// Limex
typedef S<Not, S<Sum, S<GT, U<0>, S<N, Z>>, S<GT, S<N, Z>, U<0>>>> EQ_1;

/*template<typename T>
struct Limex {
  typedef typename R<S<T, Z>, S<Sum, U<2>, S<EQ_1, S<T, U<1>>>>> Impl;
  typedef typename S<GZ, S<Impl, Z, U<0>>> Exec;
};*/

template<typename T>
using LimexImpl = R<S<T, Z>, S<Sum, U<2>, S<EQ_1, S<T, U<1>>>>>;

template<typename T>
using Limex = S<GZ, S<LimexImpl<T>, Z, U<0>>>;



// Start functions
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

int pow(int x, int y) {
  return Pow::res(x, y);
}

int gt(int x, int y) {
  return GT::res(x, y);
}

int prime(int x) {
  return Prime::res(x);
}

template<typename T>
int limex(int y) {
  return Limex<T>::res(y);
}
// End functions

int main() {
  
  using std::cout;
  using std::endl;

  cout << sum(2, 3) << "\n"; // 5
  cout << mul(2, 3) << "\n"; // 6
  cout << sub(10, 4) << "\n"; // 6
  cout << idiv(10, 2) << " " << rem(9, 5) << "\n"; // 5 4
  cout << pow(2, 3) << "\n"; //8
  cout << gt(1, 1) << " " << gt(0, 1) << " " << gt(5, 2) << "\n"; // 0 0 1
  cout << prime(6) << " " << prime(7) << "\n"; // 0 1
  cout << limex<GZ>(1) << " " << limex<GZ>(4) << "\n"; // 0 1

  return 0;
}
