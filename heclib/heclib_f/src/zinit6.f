      SUBROUTINE zinit6
C
C
C     Initialize all parameters required for DSS Software
C
C     DSS File Structure Level 6:  Written by Bill Charley and
C                                  Art Pabst, HEC 1988
C
C     In the DSS Software (subroutines that begin with the letter 'Z')
C     Variables that begin with the letter 'K' are pointers
C     Variables that begin with the letter 'C' are character variables
C     Variables that begin with the letter 'L' are logicals
C     Variables that begin with the letter 'N' are often counters, or
C        string lengths
C     Other variables follow standard FORTRAN defaults
C
C     File Structure:
C        Level 6 has two file structures.  The first is the default
C     one, designed for files that are dynamic with regards to the
C     number of records they may contain.  The second is for more
C     stable data bases, where the number of records are pretty
C     constant (e.g., a district Master Data Base).
C        In the first structure, the first part of the file (224 words)
C     contains the permanent section of file.  This contains information
C     such as the number of records, file size, file structure, last
C     written date and time, etc.  Following the permanent section is a
C     "hash table", whose length is twice the hash size (settable by
C     the zset6 "SIZE" parameter.  Following this is a block of "pathname
C     bins" (about 20 bins per block), then a data area.  As a file
C     grows, more bins are added to the end of the file, as needed.
C        Data is stored or retrieved in the following manner:
C     The subroutine zhash6 computes a hash code from the pathname
C     (always the same for the same pathname), a number between one and
C     the hash size.  This number indexes a position in the hash table
C     which points (with a two word address) to a pathname bin.  The
C     software reads this bin and searches it for the pathname (a bin
C     may contain 6 or 7 pathnames).  If found, an associated address
C     points to the data location.  If not found, an address points
C     to an over flow bin which is read and searched, until there
C     are no more pathnames for this hash code.  Ideally, there should
C     be few over flow bins.
C        The second file structure is similar to the first, except that
C     there is no hash table.  Instead, a pathname bin is reserved for
C     each possible hash code.  Thus on opening a new file, the file
C     is somewhat large (with reserved bins), but one less disk access
C     is needed (no hash table) when accessing data.
*
*
*  DSS File Structure:
*        1.  Root (Perm) area
*        2.  Index Table (optional)
*        3.  Pathname Bin Block
*        4.  Data Area
*        5.  Additional Pathname Bins
*        6.  Additional Data Area
*        7.  etc.
*
*  Description of Root (Perm) Area:
*     The root area identifies the file as a DSS file, and
*     contains information such as the version number, file
*     size, last written date, etc..
*        1.  KDSS    the identifier 'ZDSS'
*        2.  KNRECS  number of records in file
*        3.  KSEQNO  catalog sequence number
*        4.  KHSIZE  The Hash size code from zfsize6
*        5.  KVERS   DSS version '6-AA'
*        6.  KFSIZE  File size in words
*        7.  KDEAD   Amount of dead space
*        8.  KCREAT  File creation date
*        9.  KLWDAT  Last written date
*       10.  KLWTIM  Last written time
*       11.  KTAGBK  The address of the tag-hash code block
*       12.  KHASH   Maximum Hash value (e.g., 512)
*       13.  KTABLE  Flag if hash table used:
*                      1:  Table
*                      2:  No table (go directly to path bins)
*       14.  KBNBLK  Number of path bins per block (e.g., 16)
*       15.  KBNREM  Number of bins remaining in current block
*       16.  KBNSIZ  Size of each bin (in words) (e.g., 224)
*       17.  KAFBIN  Address of the first bin in file
*       18.  KANBIN  Address of the next available bin
*       19.  KTAGS   Record tag scheme
*     The following 5 are for file effiency information only
*       20.  KBINS   Number of bins used
*       21.  KHUSED  Number of hash codes used
*       22.  KBOVER  Number of bins that have overflowed
*       23.  KMAXPH  Maximum number of pathnames for any one hash code
*       24.  KMAXHC  The hash code for KMAXPH
*       25.  KCOMPI  The address of compression information
*       26.  KCOMPN  The length of the compression information
*       27.  KITSIN  The amount to increment an irreg. T.S. block by
*       28.  KITSDA is the minimum block size for the I.T.S. day block
*       29.  KITSMO is the minimum block size a month block
*       30.  KITSYE is the minimum block size a year block
*       31.  KITSDE is the minimum block size a decade block
*       32.  KITSCE is the minimum block size a century block
*       33.  KFPASS is the file (super user's) password
*       34.  KMXPRT is the maximum part lengths for all pathnames
*       35.  KCOLL is a flag indicating file has collections
*       36.  KRES - unused area - 14 words
*
*  Index Table:
*     This contains the addresses for the pathname bins corresponding
*     to the hash codes.  This table (if used) follows the root (perm)
*     section and is the same length as the maximum number of hash codes
*     For example:  If the maximum hash code is 512, (all pathnames will
*     yield a hash code between 1 and 512), a pathnames hash code is
*     looked up in the table to find the address of the pathname bin for
*     that code.  The bin will contain all pathnames with that hash code
*     and where that data lives.  (If the bin fills up, it points to a
*     new (overflow) bin.  If the index table is not used
*     (IFLTAB(KTABLE) = 2), then 512 bins would be written to the
*     beginning of the file, and the hash code would point to the
*     bin directly (on less read to access data, but alot more
*     space taken up).
*
*  Pathname Bins and Blocks:
*     Several bins (e.g., 16) are stored together to make up one
*     block.  Each BIN consists of:
*        1.  KBSTAT  Status parameter:
*                       = -1, Bin overflows into another bin
*                       = 0, No more pathanmes for this hash
*                       = 1, pathname follows
*                       = 2, following pathname has been deleted
*                       = 3, following pathname has been renamed
*                       = 4, following pathname has been replaced
*                            (deleted, then written over)
*                       = 11, Pathname follows, but is long version
*                             (> 80 chars, or parts > 32 chars)
*                       = 12, following long pathname has been deleted
*                       = 13, following long pathname has been renamed
*        2.  KBNPAT  Number of characters in this pathname
*        3.  KBPATH  Pathname
*        4.  KBAINF  Address of its infromation (and data) block
*        5.  KBNHEA  Number of its header words
*        6.  KBNDAT  Number of data words
*        7.  KBLNDA  Logical number of data available e.g., uncompressed
*        8.  KBTYPE  The data type (e.g., time-series)
*        9.  KBTAG   Record tag id
*       10.  KBHASH  The hash code used for this record
*       11.  KBPINTL pseudo interval, if pseudo regular interval
*                    time series (irregular interval record)
*        ... repeats for as many pathnames as will fit ...
*        End-of-bin minus one.  Address of next BIN (for this hash)
*        End-of-bin.  Address of next BLOCK (used for catalog only!)
*
*  Data Area:
*     Each record's data area contains an information (info) block,
*     a header area, and the data.  In additon, there can be areas
*     for data compression information, and data quality flags.
*     The INFO block is comprised of the following:
*        1.  KIFLAG  Flag (-9753) indicating info block
*        2.  KISTAT  Status flag:
*                       = -1, Data lives elsewhere (size increased)
*                       = 1, pathname follows (status ok)
*                       = 2, following pathname has been deleted
*                       = 3, following pathname has been renamed
*                       = 4, following pathname has been replaced
*                            (deleted, then written over)
*                       = 11, Pathname follows, but is long version
*                             (> 80 chars, or parts > 32 chars)
*                       = 12, following long pathname has been deleted
*                       = 13, following long pathname has been renamed
*        3.  KINPAT  Number of characters in pathname
*        4.  KIPATH  Pathname
*        5.  KIPASS  Record Password
*        6.  KIADAT  Address of data
*        7.  KINDAT  Size of data
*        8.  KILNDA  Logical number of data (e.g., uncompressed)
*        9.  KIVER   Version of data (times written)
*       10.  KIPROG  Name of program which (last) wrote this data.
*       11.  KIDATE  Date last written to
*       12.  KITIME  Time last written to
*       13.  KITAG   Record tag id
*       14.  KITYPE  Type of data (e.g., time-series)
*       15.  KICOMP  Data compression flag
*       16.  KIQUAL  Data quality flag (or alternative flag for data)
*       17.  KIAIHE  Address of the internal header area (used to
*                    store items like units)
*       18.  KINIHE  Length of the internal header area
*       19.  KIACHE  Address of the compression header area
*       20.  KINCHE  Length of the compression header area
*       21.  KIAUHE  Address of the User's header area
*       22.  KINUHE  Length of the User's header area
*       23.  KIPREC  Precision of data (0: undefined; 1: nearest
*                    tenth; 2: nearest hundredth, etc.)
*       24.  KIUNUS  Unused space - Space in the record not used because
*                    the last version was smaller the the previous
*
*     Machine flags are set in columns 73-80.  These indicate if the
*     line should be commented out or not.  The following flags
*     are used:
*        1.  H:  Harris
*        2.  M:  Microsoft Fortran (DOS)
*        3.  L:  Lahey Fortran (DOS)
*        4.  u:  Unix (common to all unix machines)
*        5.  l:  LPI Fortran (SCO Unix)
*        6.  g:  Greenhills Fortran (Intergraph)
*        7.  m:  Mips (CDC workstations)
*        8.  c:  Cray (Unix)
*
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdssbz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'verticalDatumFortran.h'      
C
      INTEGER IERR
C
C
      EXTERNAL BKDATW
      COMMON /WORDS/ IWORD(10)
      
      INTEGER I
C
C     LOGICAL LEXIST, LOPEN
C
C
C     Iinitialize the block data (on Harris, a subroutine)
C     CALL ZBKDAT
C
C
C
C     Set the size and character parameters
C
C     NCMW is the number of characters per machine word
      NCMW = IWORD(1)
C     NCPW is the number of characters per physical word
      NCPW = IWORD(2)
C
C     NWPW is the number of machine words per physical word
      NWPW = NCPW / NCMW
C
C     Check that this common block was loaded correctly
      IF ((NCPW.LT.1).OR.(NCPW.GT.10)) THEN
      IF (MUNIT.EQ.0) MUNIT = 6
      WRITE (MUNIT,20) NCPW
 20   FORMAT (/' **** DSS **** Apparent installiation error in zinit6',
     * /,' Illegal Value for the number of characters per integer ',
     * 'word:',I8,/,' Check that block data ZBKDAT was loaded',/)
      CALL ABORT
      ENDIF
C
C     Should the byte order be reversed?  (On "big-endian" workstations
C     this should be set to -1 to switch byte orders in CHRHOL routines,
C     in order to preserve file compatability with DOS.
C     Be sure to check zrrec6 and zwrec6 for byte switching also.
C     NRECL is the record length in bytes
      call getendian(I)
      if (I.EQ.0) THEN
          IWORD(4) = 0
           NRECL = NCPW * NBSIZE
      ELSE
          IWORD(4) = -1
          NRECL = NCPW * (NBSIZE + 1)
      ENDIF
C
C     Set data type, first record version number to defaults
      ITYPE = 0
      ICOMP = 0
      IQUAL = 0
      IBVER = 1
      INTL_PSEUDO = 0
C
C
C     NSECRC is the number of sectors
C     On the Harris, the record size MUST always be a multiple of 112.
      NSECRC = (((NBSIZE * NWPW) - 1) / 112) + 1
C
C     NTAGBK is the size of the tag-hash code block
      NTAGBK = NBSIZE
C
C     CPROG is the name of the program writing records to the file
      NPROGC = 6
      NPROG = (NPROGC - 1)/NCPW + 1
C
C     CDSS is the identifier 'ZDSS', indicating a DSS file
      CDSS = 'ZDSS'
      NDSSC = 4
      NDSS = (NDSSC - 1)/NCPW + 1
C
C     NVERS is the length of the version (level) number, set in ZBKDAT
      NVERSC = 4
      NVERS = (NVERSC - 1)/NCPW + 1
C
C     NTAGC is the number of characters for the record tag
      NTAGC = 8
      NTAG = (NTAGC - 1)/NCPW + 1
C
C     NCOMP is the length allocated in IFLTAB for data compression info
      NCOMP = 75
C
C     NPFLAG is a flag indicating the start of a data information block
      NPFLAG = -9753
C
C     JTAGFL is a flag indicating  the start of a tag-hash code block
      JTAGFL = -8642
C
C     JEOFFL is a flag indicating the end of the DSS file (written in
C     the word just after the last word in the file).
      JEOFFL = -9630
C
C     Initialize read/write variables for the buffer common block
      DO 40 I=1,MXBUFF
      JCREC(I) = -1
      JWRITE(I) = 0
      JMXREC(I) = 0
      LOCKBF(I) = .FALSE.
 40   CONTINUE
      JBUFF = 0
C
C     Set the current date and time
      CALL WHEN ( CDATE, CTIME)
C     NDATEC and NTIMEC are the number of characters in CDATE and CTIME
      NDATEC = 7
      NTIMEC = 8
      NDATE = (NDATEC - 1)/NCPW + 1
      NTIME = (NTIMEC - 1)/NCPW + 1
C
C     NPASSC is the number of characters in a password
      NPASSC = 7
      NPASS = ((NPASSC - 1)/NCPW) + 1
C
C     NMXPRT is the number of words to hold the maximum
C     lenghts of the pathname parts (1 byte each)
C     NMXPRT = ((6 - 1)/NCPW) + 1
      NMXPRT = 2
C
      LDELET = .FALSE.
      LBWRIT = .FALSE.
      IRENAM = 0
C
C
C     KEY1-3 are pointers to words in IFLTAB that should
C     contain the KEY value (NKEY) after zopen6
C     If they do not, then IFLTAB has become corrupted
C     due to a program error (array overwritten).
C
      NKEY = 13579
C
C     Beginning of IFLTAB pointers
C
C     KNV is the numerical version number (i.e., 6) so applications
C     programs (DSSUTL) can quickly tell what version is being used
      KNV = 1
C
C     KVERNO points to an integer number representation of the
C            last part of the version.
      KVERNO = KNV + 1
C
      KEY1 = KVERNO + 1
C
C     KSWAP points to a flag indicating if all bytes need to be swapped,
C     because the file is a different Endian from the machine.
      KSWAP = KEY1 + 1
C
C     KDSWAP points to a flag indicating if words for DOUBLE PRECISION
C     values need to be swapped to keep files compatitable between
C     different Endian machines.  (swap on unix side)
      KDSWAP = KSWAP + 1
C
C     KUNIT points to the unit number for this file.
      KUNIT = KDSWAP + 1
C
C     KHANDL points to the handle for this file (similar to unit).
      KHANDL = KUNIT + 1
C
C     KFILES points to the number of files that have been opened.
      KFILES = KHANDL + 1
C
C     KREMOTE point to a flag that indicates if the file is on a remote
C     disk (network mounted or client server).  Not guarenteed.
C     Set to 1 if remote, 0 if local or unknown
      KREMOTE = KFILES + 1
C
C     KNAME points to the file name, for the PC only
      KNAME = KREMOTE + 1
C
C     KMULT points to the multiple user access flag
      KMULT = KNAME + 16
C
C     KMEMORY points to the flag that indicates file is all memory
      KMEMORY = KMULT + 1
C
C     KLOCK indicates if this file is currently locked
      KLOCK = KMEMORY + 1
C
C     KLOCKR indicates if the lock request word is locked
      KLOCKR = KLOCK + 1
C
C     KLOCKB points to the byte position of the lock record
      KLOCKB = KLOCKR + 1
C
C     KLOCKB points to the byte position of the lock read record
C     which indicates the file is in use and someone else cannot
C     have exclusive access
      KLOCKRD = KLOCKB + 1
C
C     KMXREC points to the maximum record that has been physically
C     written, so that later records are not written before previous
C     ones (only applies to new records).
      KMXREC = KLOCKRD + 1
C
C     KBSADD is the beginning file size pointer
C     This is the file size at the beginning of a write (KFSIZE is
C     what the size will be after the write has been completed)
      KBSADD = KMXREC + 1
C
C     KFILEW points to a flag indicating if the file has been written to
C     The value of IFLTAB will be set to 1 when it has.
      KFILEW = KBSADD + 1
C
C     KREADO points to a flag indicating if the file is read only
      KREADO = KFILEW + 1
C
C     KSTAT points to a flag indicating the status of the file when
C     a fatal error occurs
      KSTAT = KREADO + 1
C
C     KSUSER points to a flag indicating if the user is the super user
C     (i.e., their password matches the file password) (0=no, 1=yes).
      KSUSER = KSTAT + 1
C
C     KEXCL indicats if the file has been exclusivly opened
      KEXCL = KSUSER + 1
C
C     KWLOCK indicats if the file is in a write lock state
      KWLOCK = KEXCL + 1
C
C
C     Permanent seciton file pointers (this area stored on disk)
C
C     KPERM pointes to the permanent section in IFLTAB
      KPERM = KWLOCK + 1
C
C     KDSS points to the identifier 'ZDSS', indicating that this is
C     a DSS file
      KDSS = KPERM
C
C     KNRECS points to the number of records in the file
      KNRECS = KDSS + NDSS
C
C     KSEQNO points to the sequence number, used for the default tag
      KSEQNO = KNRECS + 1
C
C     KHSIZE points to the hash size code from zfsize6.
      KHSIZE = KSEQNO + 1
C
C     KVERS is the DSS software version for this file
C     KVERS must always remain in the same location in the file
C     so past and future versions can recognize the file as DSS
      KVERS = KHSIZE + 1
C
C     KFSIZE is the file size pointer (in machine words)
      KFSIZE = KVERS + NVERS
C
C     KDEAD is the dead space pointer
      KDEAD = KFSIZE + 1
C
C     KCREAT pointes to the date the file was created
      KCREAT = KDEAD + 1
C
C     KLWDAT points to the date the file was last written to
      KLWDAT = KCREAT + NDATE
C
C     KLWTIM points to the time the file was last written to
      KLWTIM = KLWDAT + NDATE
C
C     KTAGBK is the address of the tag-hash code block.  This block
C     aids in finding a pathname, given its tag.
      KTAGBK = KLWTIM + NTIME
C
C     KHASH points to the maximum hash value for this file
      KHASH = KTAGBK + 1
C
C     KTABLE indicates whether a hash table is uses (structure 1)
C     or a hash goes directly to a pathname bin.
      KTABLE = KHASH + 1
C
C     KBNBLK points to the number of bins per block (except
C     for the first block).
      KBNBLK = KTABLE + 1
C
C     KBNREM points to the number of bins remaining in the current block
      KBNREM = KBNBLK + 1
C
C     KBNSIZ points to the size of the pathname bin (in words)
      KBNSIZ = KBNREM + 1
C
C     KAFBIN points to the location of the first bin
      KAFBIN = KBNSIZ + 1
C
C     KANBIN points to the location of the next empty bin
      KANBIN = KAFBIN + 1
C
C     KTAGS points to the record tag scheme (takes up 16 words)
      KTAGS = KANBIN + 1
C
C
C     File effiency variables
C
C     KBINS indicates the number of pathname bins used in the file
      KBINS =  KTAGS + 16
C
C     KHUSED are the number of hash codes used.  This will always be
C     less than or equal to KHASH
      KHUSED = KBINS  + 1
C
C     KBOVER is the number of overflow bins - previous bins filled
      KBOVER = KHUSED + 1
C
C     KMAXPH is the maximum number of pathnames for any one bin
      KMAXPH = KBOVER + 1
C
C     KMAXHC is the hash code for this
      KMAXHC = KMAXPH + 1
C
C     KCOMPI is a pointer to compression information
      KCOMPI = KMAXHC + 1
C
C     KCOMPN is the length of this compression information
      KCOMPN = KCOMPI + 1
C
C     KITSIN is the amount to increment an irregular time seris block by
      KITSIN = KCOMPN + 1
C
C     KITSDA is the minimum block size for the I.T.S. day block
      KITSDA = KITSIN + 1
C
C     KITSMO is the minimum block size for the I.T.S. month block
      KITSMO = KITSDA + 1
C
C     KITSYE is the minimum block size for the I.T.S. year block
      KITSYE = KITSMO + 1
C
C     KITSDE is the minimum block size for the I.T.S. decade block
      KITSDE = KITSYE + 1
C
C     KITSCE is the minimum block size for the I.T.S. century block
      KITSCE = KITSDE + 1
C
C     KFPASS points to the file password (encoded)
      KFPASS = KITSCE + 1
C
C     KMXPRT points to the maximum lengths of pathname parts
      KMXPRT = KFPASS + NPASS
C
C     KCOLL points to a flag to indicate if this file has collections
      KCOLL = KMXPRT + NMXPRT
C
C     additional words are reserved after the perm
C     section for any future use
C
      KRES = KCOLL + 1
C
C     Set KEY2 location
      KEY2 = KRES + 14
C
C     Figure out NPERM
      NPERM = KEY2 - KPERM
C
C     Other information in IFLTAB
C
C     Last Pathname accessed Information
C
C     KAINFO points to the location of the last paths info block
      KAINFO = KEY2 + 1
C
C     KDTYPE points to the last paths data type (time-series, paired)
      KDTYPE = KAINFO + 1
C
C     KPADD points to the address of the last pathname's bin
      KPADD = KDTYPE + 1
C
C     KPJBIN points to the location in the last pathname's bin
      KPJBIN = KPADD + 1
C
C     KPNHEA points to the header length of the last pathname
      KPNHEA = KPJBIN + 1
C
C     KPNDAT points to the data length of the last pathname
      KPNDAT = KPNHEA + 1
C
C     KLPFOU points to word indication if the last pathname was found
      KLPFOU = KPNDAT + 1
C
C     KINTAB points to indication if last pathname's hash was in table
      KINTAB = KLPFOU + 1
C
C     KLPATL points to the length of the last pathname (in characters)
      KLPATL = KINTAB + 1
C
C     KLPATH points to the last pathname
      KLPATH = KLPATL + 1
C
C     Read Buffer information
C     KRBNPA points to the number of characters in the pathname
      KRBNPA = KLPATH + 390/NCPW + 1
C
C     KRBHBE points to the current header address (next set to be read)
      KRBHBE = KRBNPA + 1
C
C     KBRHEN points to the ending address of the header block
      KRBHEN = KRBHBE + 1
C
C     KRBDBE points to the current data address (next set to be read)
      KRBDBE = KRBHEN + 1
C
C     KBRDEN points to the ending address of the data block
      KRBDEN = KRBDBE + 1
C
C     KRBPAT points to the pathname for this read buffer
C     (Pathname size will be truncated to 160)
      KRBPAT = KRBDEN + 1
C
C
C     Write Buffer information
C     KWBNPA points to the number of characters in the pathname
      KWBNPA = KRBPAT + 160/NCPW + 1
C
C     KWBSIZ points to a flag if the data size has been pre-specified
      KWBSIZ = KWBNPA + 1
C
C     KWBHAD points to the current address of the header block
      KWBHAD = KWBSIZ + 1
C
C     KWBDAD points to the current address of the data block
      KWBDAD = KWBHAD + 1
C
C
C     KWBPAT points to the pathname for this write buffer
C     (Pathname size will be truncated to 160)
      KWBPAT = KWBDAD + 1
C
C
      KCOMP = KWBPAT + 160/NCPW + 1
C
      KEY3 = KCOMP + NCOMP
C
C
C
C     Pointers for pathname bins (generally 2nd char is 'B')
C
C     KBSTAT points to the status flag of the bin
      KBSTAT = 0
C
C     KBNPAT points to the number of characters in this pathname
      KBNPAT = KBSTAT + 1
C
C     KBPATH points to the pathname
      KBPATH = KBNPAT + 1
C
C     KBAINF points to the address of the information block
C     The following variables are used with the lenght of the pathname
C     (in words) added to them
      KBAINF = KBPATH
C
C     KBNHEA points to the number of header words stored for this record
      KBNHEA = KBAINF + 1
C
C     KBNDAT points to the number of data words stored for this record
      KBNDAT = KBNHEA + 1
C
C     KBLNDA points to the logical number of data (for example, the
C     number of uncompressed data, or data without missing flags
      KBLNDA = KBNDAT + 1
C
C     KBTYPE points to the data type (e.g., time-series)
      KBTYPE = KBLNDA + 1
C
C     KBTAG points to the record tag identifier
      KBTAG = KBTYPE + 1
C
C     KBHASH points to the hash code used for this record
      KBHASH = KBTAG + NTAG
C
C     KBPINTL is the pseudo time interval for irregular interval data.
C     If it is zero, then there is no pseudo time interval
      KBPINTL = KBHASH + 1
C
C     Figure out the number of words for each pathname in the bin,
C     excluding the pathname:
C     (actual space = NLBIN + NWPATH)
      NLBIN = KBPINTL - KBSTAT + 1
C
C
C     Data information block buffer (generally 2nd char is 'I')
C
C     KIFLAG points fo a flag indicating a information block (=-9753)
      KIFLAG = 1
C
C     KISTAT points to this records status
      KISTAT = KIFLAG + 1
C
C     KINPAT point to the number of characters in the pathname
      KINPAT = KISTAT + 1
C
C     KIPATH points to the pathname for this record
      KIPATH = KINPAT + 1
C
C     The following are added to the number of words in the pathname
C
C     KIPASS is the record password (2 words long) (encoded)
      KIPASS = KIPATH
C
C     KIADAT points to the data location
      KIADAT = KIPASS + NPASS
C
C     KINDAT points to the length of the data
      KINDAT = KIADAT + 1
C
C     KILNDA points to the logical number of data (e.g., uncompressed)
      KILNDA = KINDAT + 1
C
C     KIVER points to the record version number (number times written)
      KIVER  = KILNDA + 1
C
C     KIPROG points to the program which last wrote to this record
      KIPROG = KIVER  + 1
C
C     KIDATE, KITIME points to the date and time last written to
      KIDATE = KIPROG + NPROG
      KITIME = KIDATE + NDATE
C
C     KITAG point to the record tag
      KITAG = KITIME + NTIME
C
C     KITYPE point to the data type (e.g., time-series)
      KITYPE = KITAG + NTAG
C
C     KICOMP points to the data compression flag (=0 if no compression)
      KICOMP = KITYPE + 1
C
C     KIQUAL points to the data quality flag (or alternive data flag)
      KIQUAL = KICOMP + 1
C
C     KIAIHE points to the address of the internal header
      KIAIHE = KIQUAL + 1
C
C     KINIHE points to the length of the internal header
      KINIHE = KIAIHE + 1
C
C     KIACHE points to the address of the compression header
      KIACHE = KINIHE + 1
C
C     KINCHE points to the length of the compression header
      KINCHE = KIACHE + 1
C
C     KIAUHE points to the address of the User's header
      KIAUHE = KINCHE + 1
C
C     KINUHE points to the length of the User's header
      KINUHE = KIAUHE + 1
C
C     KIPREC points to the data precision (1=tenth, 2=hundredth)
      KIPREC = KINUHE + 1
C
C     KIUNUS is unused space in the record
      KIUNUS = KIPREC + 1
C
C     compute the lenght of the information block, less the pathname
      NINFO = KIUNUS - KIFLAG + 1
C
C
C     Should a multi-user mode be forced (LMULTU TRUE), or should we
C     allow the file be be opened in a multi-user advisory mode
C     (MUCH faster on the PC, if we don't have several users)
      LMULTU = .FALSE.
C
C     Initialize default vertical datum to UNSET
      IVDATUM = IVD_UNSET
      CVDATUM = CVD_UNSET
C
C
C     For Intel compiler and running 'java', if standard out is not
C     opened, the library aborts on the first write to standard out
C     Check with a write.  If an error, change the message unit to 7
C     and open as a scratch file.
C     There is no problem when running 'javaw', or the message file
C     is assigned.
      IF ((MLEVEL.GE.1).AND.(MUNIT.EQ.6)) THEN
        WRITE(UNIT=6,IOSTAT=IERR,FMT='(A)')' '
        IF (IERR.NE.0) THEN
            MUNIT = 7
            OPEN(UNIT=7, STATUS='SCRATCH',IOSTAT=IERR)
         ENDIF
      ENDIF
C
C     INITILIZATION COMPLETE
C
      IF (MLEVEL.GE.10) WRITE (MUNIT,820)
 820  FORMAT (T5,'-----DSS---zinit6:  Exit')
C
      RETURN
C
      END
      BLOCK DATA ZBKDAT
C     SUBROUTINE ZBKDAT
C
C     Written by Bill Charley at HEC, 1988.
C
C
      INCLUDE 'zdsskz.h'
C
      INCLUDE 'zdsslz.h'
C
      INCLUDE 'zdsscz.h'
C
      INCLUDE 'zdssnz.h'
C
      INCLUDE 'zdssiz.h'
C
      INCLUDE 'zdssmz.h'
C
      INCLUDE 'zdsscm.h'
C
      INCLUDE 'zdssts.h'
c
      INCLUDE 'zdssShared.h'
      INCLUDE 'zopenDefault.h'
C
      INCLUDE 'verticalDatumFortran.h'
C
C
      COMMON /ZDSSFZ/ LFIRST
      LOGICAL LFIRST
C
      COMMON /DCDBUG/ LDCDBG, MDCUNT
      LOGICAL LDCDBG
C
      COMMON /WORDS/ IWORD(10)
C
      COMMON /DSSMEM/ MEMARRAY(10), MEMSIZES(10)
C
      COMMON /UNDEF_TIME/ ITIME_UNDEF
      INTEGER ITIME_UNDEF
C
C
C   DO NOT CHANGE THE STYLE OF THE FIRST TWO CHARACTERS OF CVERS
C   THE FIRST CHARACTER IDENTIFIES THE VERSION NUMBER (4,5,6, ETC)
C   THE SECOND CHARACTER IDENTIFIES THE FILE AS A DSS FILE.
C
      DATA CVERS /'6-YO'/
      
      !DATA INT_MIN /-2147483648/ == 0
      DATA ITIME_UNDEF /-2147483647/
C
      DATA MLEVEL /4/
      DATA MUNIT /6/
C
      DATA LDCDBG /.FALSE./
      DATA MDCUNT /6/
C
      DATA LFIRST /.TRUE./
      DATA LREAD /.FALSE./
      DATA LWRITE /.FALSE./
      DATA LUNDEL /.FALSE./
      DATA LMAP/.FALSE./
      DATA LCCDAT/.FALSE./
      DATA LEXCL/.FALSE./
      DATA LWLOCK/.FALSE./
      DATA LPROTC/.FALSE./
      DATA LREADO/.FALSE./
      DATA LCOFIL/.FALSE./
      DATA LTOL/.FALSE./
      DATA LNOABT /.FALSE./
      DATA LUPRTIME /.TRUE./
      DATA LQPBIT /.FALSE./
      DATA LSHOWHANDL /.FALSE./
      DATA LCPEMPTY /.TRUE./
      DATA LPSEUDO /.TRUE./
      DATA LFSQUEEZE /.FALSE./
C
C     Allow files to be written to 8 GB for DSS Version 6-Q and later
      DATA L8GB /.TRUE./
C
C     FIRST UNIT TO OPEN LESS ONE
      DATA NUNIT /70/
      DATA NFILES /0/
C
C     Default hash size
      DATA IHSIZE /0/
C
C     Default data precision
      DATA IPREC /0/
C
      DATA versionNext /0/
      DATA versionAll /0/
C
C     ALTERNATIVE UNIT NUMBER (LDUNIT)
      DATA LDUNIT /.FALSE./
      DATA IDUNIT /0/
C
      DATA CPROG / 'Undefi'/
      DATA CTAG /' '/
      DATA CRNTAG /' '/
      DATA CKPATH /' '/
      DATA CERRMS /' '/
      DATA IERRMS /0/
C
C     LOGICAL NUMBER OF DATA.  IF -1, USE NDATA BY DEFAULT
      DATA NLDATA /-1/
C
C     132 COLUMN OUTPUT BY DEFAULT;  80 COL MUST BE SET
      DATA L80COL /.FALSE./
C
C     FILE SIZE PARAMETERS - SET TO DEFAULT
      DATA LSZSET /.FALSE./
      DATA LSTABL /.FALSE./
      DATA LTSCMP /.FALSE./
      DATA LCATST /.FALSE./
      DATA LSQSTA /.FALSE./
C
C     Time zone offset flag in minutes from UTC, -1 means not set.
C     Used for time series records
      DATA IRTZONE /-1/
      DATA IWTZONE /-1/
      DATA CRTZONE /' '/
      DATA CWTZONE /' '/
C
C
C     Record data types
      DATA IRTYPE(1)  /100/, CRTYPE(1)  /'RTS'/
      DATA IRTYPE(2)  /101/, CRTYPE(2)  /'RTP'/
      DATA IRTYPE(3)  /105/, CRTYPE(3)  /'RTD'/
      DATA IRTYPE(4)  /110/, CRTYPE(4)  /'ITS'/
      DATA IRTYPE(5)  /111/, CRTYPE(5)  /'ITP'/
      DATA IRTYPE(6)  /115/, CRTYPE(6)  /'ITD'/
      DATA IRTYPE(7)  /200/, CRTYPE(7)  /'PD '/
      DATA IRTYPE(8)  /205/, CRTYPE(8)  /'PDD'/
      DATA IRTYPE(9)  /300/, CRTYPE(9)  /'TXT'/
      DATA IRTYPE(10) /400/, CRTYPE(10) /'GUT'/
      DATA IRTYPE(11) /401/, CRTYPE(11) /'GU '/
      DATA IRTYPE(12) /410/, CRTYPE(12) /'GHT'/
      DATA IRTYPE(13) /411/, CRTYPE(13) /'GH '/
      DATA IRTYPE(14) /420/, CRTYPE(14) /'GAT'/
      DATA IRTYPE(15) /421/, CRTYPE(15) /'GA '/
      DATA IRTYPE(16) /430/, CRTYPE(16) /'GST'/
      DATA IRTYPE(17) /431/, CRTYPE(17) /'GS '/
      DATA NRTYPE /17/

      DATA CRDESC(1)  /'Regular-interval time series'/
      DATA CRDESC(2)  /'Regular-interval time series pattern'/
      DATA CRDESC(3)  /'Regular-interval time series doubles'/
      DATA CRDESC(4)  /'Irregular-interval time series'/
      DATA CRDESC(5)  /'Irregular-interval time series pattern'/
      DATA CRDESC(6)  /'Irregular-interval time series doubles'/
      DATA CRDESC(7)  /'Paired Data'/
      DATA CRDESC(8)  /'Paired Data doubles'/
      DATA CRDESC(9)  /'Text Data'/
      DATA CRDESC(10) /'Gridded - Undefined grid with time'/
      DATA CRDESC(11) /'Gridded - Undefined grid'/
      DATA CRDESC(12) /'Gridded - HRAP grid with time reference'/
      DATA CRDESC(13) /'Gridded - HRAP grid'/
      DATA CRDESC(14) /'Gridded - Albers with time reference'/
      DATA CRDESC(15) /'Gridded - Albers'/
      DATA CRDESC(16) /'Gridded - SHG with time reference'/
      DATA CRDESC(17) /'Gridded - SHG'/
      DATA CRDESC(18) /'Undefined data type'/
C
C
      DATA MEMSIZES /0,0,0,0,0,0,0,0,0,0/
C
C     Vertical Datums (same order as java DataContainer)
      DATA  CVD_UNSET  /'UNSET'  /, IVD_UNSET  /0/ 
      DATA  CVD_NAVD88 /'NAVD-88'/, IVD_NAVD88 /1/  
      DATA  CVD_NGVD29 /'NGVD-29'/, IVD_NGVD29 /2/   
      DATA  CVD_OTHER  /'OTHER'  /, IVD_OTHER  /3/ 
      DATA  CVD_LOCAL  /'OTHER'  /, IVD_LOCAL  /3/
      
      DATA VERTICAL_DATUM_INFO_PARAM      /'verticalDatumInfo'    /
      DATA VERTICAL_DATUM_PARAM           /'verticalDatum'        /
      DATA UNDEFINED_VERTICAL_DATUM_VALUE /-3.4028234663852886e+38/
C
C
C     Variables for INTEGER*6 on Harris
      DATA IZERO /0/
C
C
C     IWORD(1) - NCMW - NUMBER OF CHAR PER MACHINE WORD
C     IWORD(2) - NCPW - NUMBER OF CHAR PER LOGICAL WORD
C     IWORD(3) - NBCH - NUMBER OF BITS PER CHARACTER
C     IWORD(4) - MASKA - CHARACTER MASK
C     IWORD(5) - MAS - COMPLEMENT OF MASKA
C     IWORD(6) - NBMW - NUMBER OF BITS PER MACHINE WORD
C     IWORD(7) - NCMW - NUMBER OF CHARACTERS PER MACHINE WORD
C     IWORD(8) - INTEGD - NUMBER OF SINGLE INTEGER WORDS IN
C                         DOUBLE INTEGER WORD
C     IWORD(9) - IREALS - NUMBER OF SINGLE INTEGER WORDS IN
C                         SINGLE REAL WORD
C     IWORD(10) - IREALD - NUMBER OF SINGLE INTEGER WORDS IN
C                          DOUBLE REAL WORD
C
C
C     DATA FOR CDC
C     DATA IWORD/10,10,6,77B,77777777777777777700B,60,10,1,1,2/         C
C     DATA FOR HEWLETT PACKARD 9000
C     DATA IWORD/4,4,8,0,0,32,4,1,1,2/                                  P
C     DATA FOR IBM-PC USING MS FORTRAN WITH 4 BYTE WORDS
C     DATA IWORD/4,4,8,0,0,32,4,1,1,2/
C     Green Hills and LPI fortran has words set in subroutine BKDATW
C     DATA IWORD/4,4,8,0,0,32,4,1,1,2/
C     DATA FOR IBM-PC USING MS FORTRAN WITH 2 BYTE WORDS
C     DATA IWORD/2,4,8,0,0,16,2,2,2,4/
C     DATA FOR IBM
C     DATA IWORD/4,4,8,ZFF,ZFFFFFF00,32,4,1,1,2/                        I
C
C
C     RETURN
C
      END

