#ifndef _NEWTON_RAPHSON_H_
#define _NEWTON_RAPHSON_H_
#if defined(_MSC_VER)
#pragma once
#endif

/*! 
* \file log_newton_raphson.h
* \ingroup objects
* \brief This is the header file for the LogNewtonRaphson solver component class.
*
* \author Josh Lurz
* \date $Date$
* \version $Revision$
*/
#include<string>

class CalcCounter; 
class Marketplace;
class World;
class SolverInfoSet;

/*! 
* \ingroup Objects
* \brief A SolverComponent based on the newton raphson algorithm using logarithmic values. 
* \author Josh Lurz
*/

class LogNewtonRaphson: public SolverComponent {
public:
    LogNewtonRaphson( Marketplace* marketplaceIn, World* worldIn, CalcCounter* calcCounterIn );
    ~LogNewtonRaphson();
    void init();
    static const std::string& getNameStatic();
    ReturnCode solve( const double solutionTolerance, const double edSolutionFloor, const int maxIterations, SolverInfoSet& solverSet, const int period );
protected:
    const std::string& getName() const;
    static const std::string SOLVER_NAME;
};

#endif // _NEWTON_RAPHSON_H_