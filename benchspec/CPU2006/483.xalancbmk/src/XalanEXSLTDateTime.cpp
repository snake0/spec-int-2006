/*
 * Copyright 1999-2004 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#include "XalanEXSLTDateTime.hpp"
#include "XalanEXSLTDateTimeImpl.hpp"



#include <ctime>
#include <cstdio>
#ifdef SPEC_CPU
using std::sprintf;
#endif



#include <XalanMessageLoader.hpp>



#include <XObjectFactory.hpp>


XALAN_CPP_NAMESPACE_BEGIN


static const XalanEXSLTFunctionDateTime			s_dateTimeFunction;



static const XalanDOMChar	s_dateTimeFunctionName[] =
{
	XalanUnicode::charLetter_d,
	XalanUnicode::charLetter_a,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_e,
	XalanUnicode::charHyphenMinus,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_i,
	XalanUnicode::charLetter_m,
	XalanUnicode::charLetter_e,
	0
};

static const XalanEXSLTDateTimeFunctionsInstaller::FunctionTableEntry	theFunctionTable[] =
{
	{ s_dateTimeFunctionName, &s_dateTimeFunction },
	{ 0, 0 }
};

static const XalanDOMChar	s_dateTimeNamespace[] =
{
	XalanUnicode::charLetter_h,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_p,
	XalanUnicode::charColon,
	XalanUnicode::charSolidus,
	XalanUnicode::charSolidus,
	XalanUnicode::charLetter_e,
	XalanUnicode::charLetter_x,
	XalanUnicode::charLetter_s,
	XalanUnicode::charLetter_l,
	XalanUnicode::charLetter_t,
	XalanUnicode::charFullStop,
	XalanUnicode::charLetter_o,
	XalanUnicode::charLetter_r,
	XalanUnicode::charLetter_g,
	XalanUnicode::charSolidus,
	XalanUnicode::charLetter_d,
	XalanUnicode::charLetter_a,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_e,
	XalanUnicode::charLetter_s,
	XalanUnicode::charHyphenMinus,
	XalanUnicode::charLetter_a,
	XalanUnicode::charLetter_n,
	XalanUnicode::charLetter_d,
	XalanUnicode::charHyphenMinus,
	XalanUnicode::charLetter_t,
	XalanUnicode::charLetter_i,
	XalanUnicode::charLetter_m,
	XalanUnicode::charLetter_e,
	XalanUnicode::charLetter_s,
	0
};


XObjectPtr
XalanEXSLTFunctionDateTime::execute(
			XPathExecutionContext&			executionContext,
			XalanNode*						context,
			const XObjectArgVectorType&		args,
			const LocatorType*				locator) const
{
	if (args.size() != 0)
	{
		executionContext.error(getError(), context, locator);
	}

	XPathExecutionContext::GetAndReleaseCachedString	theGuard(executionContext);

	XalanDOMString&		theResult = theGuard.get();
	
	theResult.clear();

#if defined(XALAN_STRICT_ANSI_HEADERS)
	using std::localtime;
	using std::tm;
	using std::time_t;
	using std::time;
	using std::size_t;
	using std::strftime;
	using std::gmtime;
#endif

	time_t long_time;

	time( &long_time );

	const struct tm*	strctTime = localtime(&long_time);

	if (strctTime != 0 )
	{
		// save local time value
		struct tm localTime = *strctTime;

		strctTime = gmtime(&long_time);

		if(strctTime != 0 )
		{

			const size_t	MAX_DATE_TIME_LEN = 1000;
			char stringTime[MAX_DATE_TIME_LEN + 1];

			const size_t	result = strftime(stringTime, MAX_DATE_TIME_LEN, "%Y-%m-%dT%H:%M:%S", strctTime);

			if (result != 0)
			{
				theResult.assign(stringTime);
				
				long localData = localTime.tm_year * 10000 + localTime.tm_mon * 100 + localTime.tm_mday;
				long gmtData = strctTime->tm_year * 10000 + strctTime->tm_mon * 100 + strctTime->tm_mday;

				char timeZone[MAX_DATE_TIME_LEN+1];
				
				int offset = 0; 

				if( localData == gmtData )
				{
					if(localTime.tm_hour == strctTime->tm_hour)
					{
						offset = 100; //  much bigger then any legal offset
					}
					else
					{
						offset = localTime.tm_hour - strctTime->tm_hour;
					}
				}
				else if(localData < gmtData)
				{
					offset = localTime.tm_hour - strctTime->tm_hour - 24;
				}
				else
				{
					offset = localTime.tm_hour - strctTime->tm_hour + 24;
				}

				if(offset == 100)
					sprintf(timeZone , "%s", "z");
				else
					sprintf(timeZone , "%2.2d:00",offset);

				theResult.append(timeZone);
			}
		}
	}

	return executionContext.getXObjectFactory().createString(theResult);
}



const XalanDOMString
XalanEXSLTFunctionDateTime::getError() const
{
		return XalanMessageLoader::getMessage(XalanMessages::EXSLTFunctionAcceptsOneArgument_1Param, s_dateTimeFunctionName);
}



void
XalanEXSLTDateTimeFunctionsInstaller::installLocal(XPathEnvSupportDefault&		theSupport)
{
	doInstallLocal(s_dateTimeNamespace, theFunctionTable, theSupport);
}



void
XalanEXSLTDateTimeFunctionsInstaller::installGlobal()
{
	doInstallGlobal(s_dateTimeNamespace, theFunctionTable);

}



void
XalanEXSLTDateTimeFunctionsInstaller::uninstallLocal(XPathEnvSupportDefault&	theSupport)
{
	doUninstallLocal(s_dateTimeNamespace, theFunctionTable, theSupport);
}



void
XalanEXSLTDateTimeFunctionsInstaller::uninstallGlobal()
{
	doUninstallGlobal(s_dateTimeNamespace, theFunctionTable);
}



XALAN_CPP_NAMESPACE_END
