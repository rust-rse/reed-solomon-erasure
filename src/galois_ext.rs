use poly::Polynom;
use poly_math::*;
use std::fmt;

pub struct ExtendedFieldElement {
    rep_polynom: Polynom,
    reduced: bool,
}

impl ExtendedFieldElement {
    pub fn new() -> ExtendedFieldElement {
        ExtendedFieldElement {
            rep_polynom: polynom![0],
            reduced: false,
        }
    }

    pub fn reduce(&mut self) {
        let (_, reduced_rep_polynom) = self.rep_polynom.div(&EXT_POLY!());
        self.rep_polynom = reduced_rep_polynom;
    }

    pub fn from(original_rep_polynom: &Polynom) -> ExtendedFieldElement {
        
        let mut ext_field_elm :  ExtendedFieldElement = ExtendedFieldElement::new();
        ext_field_elm.rep_polynom = original_rep_polynom.clone();
        
        ext_field_elm.reduce();

        ext_field_elm
    }


    pub fn is_zero(self: &Self)-> bool {
        self.rep_polynom.is_zero()
    }

    pub fn inverse(self: &Self) -> ExtendedFieldElement {
        if !self.is_zero() {
            let (gcd, x, _) = self.rep_polynom.egcd(&EXT_POLY!());
            //we still need to normilze it by divinig by the gcd
            if !gcd.is_zero() {
                let _normalizer = ::galois_8::div(1, gcd[gcd.len()-1]);
                let normal_x = x.mul(&[_normalizer]);
                let inverted_element = ExtendedFieldElement::from(&normal_x);
                return inverted_element;
            }
        }
        //either self is zero polynomial or is equivalent to 0
        panic!("0 is not invertable");
    }
}

impl<'a> From<&'a Polynom> for ExtendedFieldElement {
    #[inline]
    fn from(polynom: &'a Polynom) -> ExtendedFieldElement {
        let mut field_element = ExtendedFieldElement::new();
        field_element.rep_polynom = polynom.clone();
        field_element
    }
}

impl PartialEq for ExtendedFieldElement {
    fn eq(&self, other: &ExtendedFieldElement) -> bool {
        self.rep_polynom == other.rep_polynom
    }
}

impl fmt::Debug for ExtendedFieldElement {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "{:?}", &self.rep_polynom)
    }
}

pub fn add(x: ExtendedFieldElement, y: ExtendedFieldElement) -> ExtendedFieldElement {
    let mut addition_result = ExtendedFieldElement::from(&x.rep_polynom.add(&y.rep_polynom));
    addition_result.reduce();
    addition_result
}

pub fn mul(x: ExtendedFieldElement, y: ExtendedFieldElement) -> ExtendedFieldElement {
    let mut mul_result = ExtendedFieldElement::from(&x.rep_polynom.mul(&y.rep_polynom));
    mul_result.reduce();
    mul_result
}

pub fn div(x: ExtendedFieldElement, y: ExtendedFieldElement) -> ExtendedFieldElement {
    let mut div_result = ExtendedFieldElement::from(&x.rep_polynom.mul(&y.inverse().rep_polynom));
    div_result.reduce();
    div_result
}

#[cfg(test)]
mod tests {
    use poly_math::*;
    use galois_ext::*;

    #[test]
    fn test_add_random_elements () {
        //e1 = (a^7 + a^6 + a^4 + a)*b + a^3 + a^2 + a + 1
        //e2 = (a^7 + a^5 + a^2)*b + a^7 + a^4 + a^3 + a
        //e1_plus_e2 = e1 + e2 #(a^6 + a^5 + a^4 + a^2 + a)*b + a^7 + a^4 + a^2 + 1
        let e1_poly = polynom![8+4+2+1, 128+64+16+2];
        let e2_poly = polynom![128+16+8+2, 128+32+4];
        let e1_plus_e2_poly = polynom![128 + 16 + 4 + 1, 64 + 32 + 16 + 4 + 2];

        let e1 = ExtendedFieldElement::from(&e1_poly);
        let e2 = ExtendedFieldElement::from(&e2_poly);
        let e1_plus_e2 = ExtendedFieldElement::from(&e1_plus_e2_poly);

        assert_eq!(add(e1, e2) , e1_plus_e2);

    }

    #[test]
    fn test_mul_random_elements () {
        
        //e1 = (a^7 + a^6 + a^4 + a)*b + a^3 + a^2 + a + 1
        //e2 = (a^7 + a^5 + a^2)*b + a^7 + a^4 + a^3 + a
        //e1_x_e2 = e1 * e2 #(a^4 + a^2 + a + 1)*b + a^7 + a^5 + a^3 + a
        let e1_poly = polynom![128+64+16+2, 8+4+2+1];
        let e2_poly = polynom![128+32+4, 128+16+8+2];
        let e1_x_e2_poly = polynom![16 + 4 + 2 + 1, 128+32+8+2];

        let e1 = ExtendedFieldElement::from(&e1_poly);
        let e2 = ExtendedFieldElement::from(&e2_poly);
        let e1_x_e2 = ExtendedFieldElement::from(&e1_x_e2_poly);

        assert_eq!(mul(e1, e2) , e1_x_e2);

    }

    #[test]
    fn test_base_poly_divid_by_no_monic_div () {
        let px = [5, 10];
        let py = [3, 9, 17, 24, 75];

        let (q, r) = py.div(&px);
        let exp_q = polynom![128+64+32+16+4, 3,3, 128+64+32+16+4+1];
        let exp_r = polynom![71];
        assert_eq!(q , exp_q);
        assert_eq!(r , exp_r);

    }

    #[test]
    fn test_inverse_of_base_element_embeded_in_extension () {
        let poly_a = polynom![2];
        let poly_1_over_a = polynom![128+8+4+2];//a^7 + a^3 + a^2 + a

        let e_a = ExtendedFieldElement::from(&poly_a);
        let e_1_over_a = ExtendedFieldElement::from(&poly_1_over_a);

        assert_eq!(e_a.inverse(), e_1_over_a);
    }

    #[test]
    fn test_inverse_of_generator() {
        //1/b = (a^4 + a^3 + a + 1)*b + a^5 + a^4 + a^2 + a
        let field_gen_poly = polynom![1,0];
        let inv_field_gen_poly = polynom![16+8+2+1, 32+16+4+2];

        let field_gen = ExtendedFieldElement::from(&field_gen_poly);
        let inv_field_gen = ExtendedFieldElement::from(&inv_field_gen_poly);
        let field_one = polynom![1];
        assert_eq!(field_gen.inverse(), inv_field_gen);
        assert_eq!(mul(field_gen,inv_field_gen), ExtendedFieldElement::from(&field_one));
        
    }
    #[test]
    fn test_div_random_elements () {
        //e1 = (a^7 + a^6 + a^4 + a)*b + a^3 + a^2 + a + 1
        //e2 = (a^7 + a^5 + a^2)*b + a^7 + a^4 + a^3 + a
        //e1/e2 = (a^7 + a^6 + a^5 + a^4 + a^3 + a^2 + 1)*b + a^6 + a^3 + a
        let e1_poly = polynom![128+64+16+2, 8+4+2+1];
        let e2_poly = polynom![128+32+4, 128+16+8+2];
        let e1_div_e2_poly = polynom![128 + 64 + 32 + 16 + 8+ 4+1, 64+8+2];

        let e1 = ExtendedFieldElement::from(&e1_poly);
        let e2 = ExtendedFieldElement::from(&e2_poly);
        let e1_div_e2 = ExtendedFieldElement::from(&e1_div_e2_poly);

        let test_div = div(e1, e2);

        assert_eq!(test_div , e1_div_e2);

    }
}