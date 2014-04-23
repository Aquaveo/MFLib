//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef H5INITIALIZE_H
#define H5INITIALIZE_H

class H5Initialize
{
public:
static void Init();

private:
  H5Initialize(const H5Initialize &rhs);
  const H5Initialize& operator= (const H5Initialize &rhs);
};

#endif