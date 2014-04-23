//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CONVERTER_H
#define CONVERTER_H

#include <iostream>

class ConverterT;

class Converter
{
friend ConverterT;
public:
  Converter(const char * const a_inputFile,
            std::ostream &a_out,
            const char * const a_outFile="");
  ~Converter();

  bool DoConversion();

private:
  Converter(const Converter &rhs);
  const Converter& operator=(const Converter &rhs);

  class impl;
  impl *m_p;
};

#endif
