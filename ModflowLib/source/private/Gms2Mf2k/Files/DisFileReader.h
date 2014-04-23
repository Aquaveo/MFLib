//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef DISFILEREADER_H
#define DISFILEREADER_H

class DisFileReader
{
public:
  explicit DisFileReader(const char * const a_, bool a_unstructured=false);
  ~DisFileReader();

  bool ReadFile();
  bool GetNumRow(int* a_nRow) const;
  bool GetNumCol(int* a_nCol) const;
  bool Unstructured() const;

private:
  DisFileReader(const DisFileReader &rhs);
  const DisFileReader &operator=(const DisFileReader &rhs);

  class impl;
  impl *m_p;

};

#endif
