      subroutine cpmcx
      write(6,*) 'the real CPMCHF file is not included in GAMESS,'
      write(6,*) 'so please use method=seminum in your $force group'
      call abrt
      return
      end
