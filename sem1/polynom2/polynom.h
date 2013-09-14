#pragma once

#include <iostream>

struct Term
{
    int power;
    double factor;
    Term(double newFactor = 0.0, int newPower = 0): power(newPower), factor(newFactor) {}
    bool operator == (const Term &term) const
    {
        return power == term.power && factor == term.factor;
    }
    bool operator != (const Term &term) const
    {
        return power != term.power || factor != term.factor;
    }
    bool operator < (const Term &term) const
    {
        return power < term.power;
    }
    friend std::ostream &operator << (std::ostream &os, const Term &term);
};

class Polynom
{
public:
    Polynom();
    Polynom(const Polynom &p);
    bool operator != (const Polynom &p) const;
    bool operator == (const Polynom &p) const;
    double operator () (const double point) const;
    Polynom &operator += (const Term &term);
    Polynom operator + (const Term &term);
    friend Polynom operator + (const Polynom &a, const Polynom &b);
    friend std::ostream &operator << (std::ostream &os, const Polynom &p);
private:
    static const int maxSize = 50;
    int size;
    Term polynom[maxSize];

    int getSize() const;
    Term operator [] (const int index) const;

    void print();
};
