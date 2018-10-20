package lab6_lab7.multipleinheritance;


class C implements X {
        X ob1, ob2;

        C(X ob1, X ob2) {
            this.ob1 = ob1;
            this.ob2 = ob2;
        }

        @Override
        public void met1() {
            ob1.met1();
        }

        @Override
        public int met2() {
            return ob2.met2();
        }
    }