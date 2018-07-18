//  (C) Copyright Aquaveo 2014. Distributed under the ModflowLib
//  Software License, Version 1.0. (See accompanying file
//  LICENSE_1_0.txt or copy at http://www.modflow.org/LICENSE_1_0.txt)
#ifndef CELLNUMBERING_H
#define CELLNUMBERING_H

#include <vector>
#include <private/util/util.h>

class CellNumberingImpl;
namespace MfData
{
  class MfGlobal;

  namespace Export
  {

    class CellNumbering
    {
      friend CellNumberingImpl;
    public:
      static CellNumbering* New(MfGlobal* a_g);

      virtual ~CellNumbering();

      virtual CStr CellIdStringFromId (int id) = 0;
      virtual int  IdFromIjk (int i, int j, int k) = 0;
      virtual int  IdInLayerFromIjk (int i, int j, int k) = 0;
      virtual void IjkFromId (int& i, int& j, int& k, int id) = 0;
      virtual void IdInLayerFromId (int& idInLay, int& lay, int id) = 0;
      virtual int  LayerFromId (int id) = 0;
      virtual int  GetStackedNumberOfCellsPerLayer () = 0;

      virtual void AdjCellInfoFromId (int id, std::vector<int>& a_adjCellIds,
        std::vector<int>& a_adjCellLayer, std::vector<Real>& a_adjCellFaceWidth,
        std::vector<Real>& a_adjCellLength) = 0;

    protected:
      CellNumbering() {}
      CellNumbering(const CellNumbering &rhs);
      const CellNumbering& operator=(const CellNumbering &rhs);
    };
  }
}

#endif
