//========================================================================
//
//  CCHANNEL.CC - part of
//                         OMNeT++
//              Discrete System Simulation in C++
//
//   Member functions of
//    cChannel : channel class
//
//  Author: Andras Varga
//
//========================================================================

/*--------------------------------------------------------------*
  Copyright (C) 1992-2003 Andras Varga

  This file is distributed WITHOUT ANY WARRANTY. See the file
  `license' for details on this and other legal matters.
*--------------------------------------------------------------*/

#include "cmessage.h"
#include "cmodule.h"
#include "carray.h"
#include "cpar.h"
#include "cenvir.h"
#include "random.h"
#include "distrib.h"
#include "csimul.h"
#include "macros.h"
#include "cgate.h"
#include "cexception.h"
#include "cchannel.h"

//=== registration
Register_Class(cChannel);
Register_Class(cSimpleChannel);

//=========================================================================
//=== cChannel - member functions

cChannel::cChannel(const cChannel& ch) : cObject()
{
    parlistp = NULL;
    linkp = NULL;
    fromgatep = NULL;

    setName( ch.name() );
    operator=( ch );
}

cChannel::cChannel(const char *name, cLinkType *link) : cObject( name )
{
    parlistp = NULL;
    fromgatep = NULL;
    linkp = link;
}

cChannel::~cChannel()
{
    // owned objects are deleted by ~cObject()
}

void cChannel::info(char *buf)
{
    cObject::info( buf );

    // find end of string and append a space
    char *b = buf;
    while(*b) b++;
    *b++ = ' '; *b='\0';

    // append useful info
    //TBD
}

void cChannel::forEach( ForeachFunc do_fn )
{
    if (do_fn(this,true))
        if (parlistp) parlistp->forEach( do_fn );
    do_fn(this,false);
}

void cChannel::writeContents(ostream& os)
{
    if (parlistp)
    {
        os << "  parameter list:\n";
        parlistp->writeContents( os );
    }
    else
    {
        os << "  no parameter list\n";
    }
}

cChannel& cChannel::operator=(const cChannel& ch)
{
    if (this==&ch) return *this;

    cObject::operator=(ch);

    fromgatep = NULL;

    if (parlistp && parlistp->owner()==this)
        discard(parlistp);
    parlistp = ch.parlistp;
    if (parlistp->owner()==const_cast<cChannel*>(&ch))
        take(parlistp = (cArray *)parlistp->dup());

    return *this;
}

void cChannel::_createparlist()
{
    parlistp = new cArray( "parameters", 5, 5 );
    take( parlistp );
}

cPar& cChannel::par(int n)
{
    cArray& parlist = parList();
    cPar *p = (cPar *)parlist[n];
    if (!p)
        throw new cException(this,"has no parameter #%d",n);
    return *p;
}

cPar& cChannel::par(const char *s)
{
    cArray& parlist = parList();
    cPar *p = (cPar *)parlist.get(s);
    if (!p)
        throw new cException(this,"has no parameter called `%s'",s);
    return *p;
}

cArray& cChannel::parList()
{
    if (!parlistp)
        _createparlist();
    return *parlistp;
}

int cChannel::findPar(const char *s) const
{
    if (!parlistp) return -1;
    return parlistp->find( s );
}

void cChannel::deliver(cMessage *msg, simtime_t t)
{
    // hand over msg to next gate
    fromGate()->toGate()->deliver(msg, t);
}

//=========================================================

cSimpleChannel::cSimpleChannel(const cSimpleChannel& ch) : cChannel()
{
    disabledp = errorp = delayp = dataratep = NULL;
    transm_finishes = 0.0;

    setName( ch.name() );
    operator=( ch );
}

cSimpleChannel::cSimpleChannel(const char *name, cLinkType *linkp) : cChannel(name, linkp)
{
    disabledp = errorp = delayp = dataratep = NULL;
    transm_finishes = 0.0;

    // set up params using link
    if (linkp)
    {
        cPar *p;
        //p = linkp->createDisabled();
        //if (p) setDisabled(p);

        p = linkp->createDelay();
        if (p) setDelay(p);

        p = linkp->createError();
        if (p) setError(p);

        p = linkp->createDataRate();
        if (p) setDatarate(p);
    }
}

cSimpleChannel::~cSimpleChannel()
{
    // owned objects are deleted by ~cObject()
}

void cSimpleChannel::info(char *buf)
{
    cChannel::info( buf );

    // find end of string and append a space
    char *b = buf;
    while(*b) b++;
    *b++ = ' '; *b='\0';

    // append other info
    //TBD
}

void cSimpleChannel::forEach( ForeachFunc do_fn )
{
    if (do_fn(this,true))
    {
        if (parlistp) parlistp->forEach( do_fn );
    }
    do_fn(this,false);
}

void cSimpleChannel::writeContents(ostream& os)
{
    cChannel::writeContents( os );

    if (dataratep)
    {
        os << "  transmission state: " << (isBusy() ? "busy" : "free") << '\n';
        if (isBusy())
            os << "  transmission finishes: " << simtimeToStr(transm_finishes) << '\n';
    }
}

cSimpleChannel& cSimpleChannel::operator=(const cSimpleChannel& ch)
{
    if (this==&ch) return *this;

    cChannel::operator=(ch);

    cArray& parlist = _parList();
    disabledp = (cPar *)parlist.get("disabled");
    delayp = (cPar *)parlist.get("delay");
    errorp = (cPar *)parlist.get("error");
    dataratep = (cPar *)parlist.get("datarate");

    return *this;
}

/*
void cSimpleChannel::setDisabled(cPar *p)
{
    p->setName("disabled");
    _parList().set(p);
    disabledp = p;
}
*/

void cSimpleChannel::setDelay(cPar *p)
{
    if (!p)
    {
        delete _parList().remove("delay");
        delayp = NULL;
        return;
    }
    p->setName("delay");
    _parList().set(p);
    delayp = p;
}

void cSimpleChannel::setError(cPar *p)
{
    if (!p)
    {
        delete _parList().remove("error");
        errorp = NULL;
        return;
    }
    p->setName("error");
    _parList().set(p);
    errorp = p;
}

void cSimpleChannel::setDatarate(cPar *p)
{
    if (!p)
    {
        delete _parList().remove("datarate");
        dataratep = NULL;
        return;
    }
    p->setName("datarate");
    _parList().set(p);
    dataratep = p;
}

cPar& cSimpleChannel::addPar(const char *s)
{
    cPar *p = new cPar(s);
    _parList().set(p);
    if (!opp_strcmp(s,"disabled"))
        disabledp = p;
    else if (!opp_strcmp(s,"delay"))
        delayp = p;
    else if (!opp_strcmp(s,"error"))
        errorp = p;
    else if (!opp_strcmp(s,"datarate"))
        dataratep = p;

    return *p;
}

cPar& cSimpleChannel::addPar(cPar *p)
{
    _parList().set(p);
    const char *s = p->name();
    if (!opp_strcmp(s,"disabled"))
        disabledp = p;
    else if (!opp_strcmp(s,"delay"))
        delayp = p;
    else if (!opp_strcmp(s,"error"))
        errorp = p;
    else if (!opp_strcmp(s,"datarate"))
        dataratep = p;
    return *p;
}

bool cSimpleChannel::isBusy() const
{
    return simulation.simTime()<transm_finishes;
}

void cSimpleChannel::deliver(cMessage *msg, simtime_t t)
{

/*
    FIXME: ev.messageSent() would crash if we delete the msg here.

    // if channel is disabled, delete message
    if (disabledp && (long)(*disabledp)!=0)
    {
        delete msg;
        return;
    }
*/

    // transmission delay modelling
    double datarate;

    if (dataratep && (datarate = (double)(*dataratep))!=0.0)
    {
        if (t < transm_finishes)     // must wait until previous
            t = transm_finishes;     //   transmissions end
        t += (simtime_t) (msg->length() / datarate);
        transm_finishes = t;
    }

    // propagation delay modelling
    if (delayp)
        t += (simtime_t) (*delayp);

    // bit error rate modelling
    if (errorp)
        if( dblrand() < 1.0 - std::pow(1.0-(double)(*errorp), (double)msg->length()) )
            msg->setBitError(true);

    // hand over msg to next gate
    fromGate()->toGate()->deliver(msg, t);
}

