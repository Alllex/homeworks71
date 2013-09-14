#include "polynom.h"
#include <cmath>

Polynom::Polynom():
    size(0)
{
}

Polynom::Polynom(const Polynom &p):
    size(p.getSize())
{
    for (int i = 0; i < size; ++i)
    {
        polynom[i] = p[i];
    }
}

bool Polynom::operator !=(const Polynom &p) const
{
    if (size != p.getSize())
    {
        return true;
    }
    for (int i = 0; i < size; ++i)
    {
        if (polynom[i] != p[i])
        {
            return true;
        }
    }
    return false;
}

bool Polynom::operator ==(const Polynom &p) const
{
    return !(*this != p);
}

Polynom operator +(const Polynom &a, const Polynom &b)
{
    Polynom q;
    for (int i = 0; i < a.getSize(); ++i)
    {
        q += a[i];
    }
    for (int i = 0; i < b.getSize(); ++i)
    {
        q += b[i];
    }
    return q;
}

double Polynom::operator ()(const double point) const
{
    int currenPower = 0;
    double result = 0.0;
    double placeHolder = 1.0;
    for (int i = size - 1; i >= 0; --i)
    {
        while (currenPower < polynom[i].power)
        {
            currenPower++;
            placeHolder *= point;
        }
        result += placeHolder * polynom[i].factor;
    }
    return result;
}

int Polynom::getSize() const
{
    return size;
}

Term Polynom::operator [](const int index) const
{
    return polynom[index];
}

Polynom &Polynom::operator +=(const Term &term)
{
    *this = *this + term;
    return *this;
}

Polynom Polynom::operator +(const Term &term)
{
    int index = 0;
    while (term < polynom[index] && index < size)
    {
        index++;
    }
    if (index == size)
    {
        polynom[size++] = term;
        return Polynom(*this);
    }
    if (term.power == polynom[index].power)
    {
        polynom[index].factor += term.factor;
        return Polynom(*this);
    }
    for (int i = size++; i > index; --i)
    {
        polynom[i] = polynom[i - 1];
    }
    polynom[index] = term;
    return Polynom(*this);
}

void Polynom::print()
{
    for (int i = 0 ; i < size; ++i)
    {
        std::cout << "(" << polynom[i].factor << ", " << polynom[i].power << ") ";
    }
    std::cout << std::endl;
}

std::ostream &operator <<(std::ostream &os, const Polynom &p)
{
    if (p.getSize() < 1)
    {
        os << "0";
        return os;
    }
    os << p[0];
    for (int i = 1; i < p.getSize(); ++i)
    {
        os << " + " << p[i];
    }
    return os;
}

std::ostream &operator <<(std::ostream &os, const Term &term)
{
    const int eps = 1e-10;
    if (std::abs(term.factor - 1.0) > eps)
    {
        if (term.factor < 0.0)
        {
            os << "(" << term.factor << ")";
        }
        else
        {
            os << term.factor;
        }
        if (term.power > 0)
        {
            os << "x";
        }
        if (term.power > 1)
        {
            os << "^" << term.power;
        }
    }
    else
    {
        if (term.power == 0)
        {
            os << "1";
        }
        if (term.power > 0)
        {
            os << "x";
        }
        if (term.power > 1)
        {
            os << "^" << term.power;
        }
    }
    return os;
}
