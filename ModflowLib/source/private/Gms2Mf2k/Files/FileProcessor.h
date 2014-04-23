//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef FILEPROCESSOR_H
#define FILEPROCESSOR_H

class DisFileReader;

class FileProcessor
{
public:
  FileProcessor(const char * const a_outPath,
                const DisFileReader &a_);
  ~FileProcessor();
  bool ProcessFile(const char * const a_);

private:
  FileProcessor(const FileProcessor &rhs);
  const FileProcessor &operator=(const FileProcessor &rhs);

  class impl;
  impl *m_p;
};

#endif

