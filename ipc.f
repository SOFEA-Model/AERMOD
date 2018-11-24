      module ipc_client
C***********************************************************************
C     This module implements a named pipe client to send status updates
C     to an external named pipe server for progress monitoring.
C***********************************************************************
#ifdef ENABLE_IPC
      use ifwinty
      private
      
      ! Parameters
      character(*), parameter :: lpszPipe =
     &  "\\\\.\\pipe\\AERMODStatusPipe"C
      
      ! Global Variables
      integer(HANDLE) :: hPipe
      integer(4) :: procid
      logical :: connected = .false.
            
      ! Derived Types
!DIR$ OPTIONS /WARN=NOALIGNMENT
      type :: errmsg_buffer_t
        sequence
        integer(4)    :: cbsize      ! message size
        integer(4)    :: procid      ! client process ID
        integer(4)    :: msgtyp = 1  ! message type
        character(2)  :: pathwy
        character(1)  :: errtyp
        character(3)  :: errcod
        integer(4)    :: lineno
        character(12) :: modnam
        character(50) :: errmg1
        character(12) :: errmg2
      end type errmsg_buffer_t
      
      type :: tothrs_buffer_t
        sequence
        integer(4)    :: cbsize      ! message size
        integer(4)    :: procid      ! client process ID
        integer(4)    :: msgtyp = 2  ! message type
        integer(4)    :: tothrs
      end type tothrs_buffer_t
!DIR$ END OPTIONS

      ! Public Procedures
      public :: ipc_connect
      public :: ipc_disconnect
      public :: ipc_send_errmsg
      public :: ipc_send_tothrs
#endif
      
      contains
      
      subroutine ipc_connect()
C***********************************************************************
C                 IPC_CONNECT
C
C        PURPOSE: Setup interprocess communication via named pipe 
C                 "\\\\.\\pipe\\AERMODProgressPipe". Uses Win32 API 
C                 declarations provided by Intel Fortran.
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    April 16, 2018
C
C        CALLED FROM:   MAIN
C***********************************************************************
#ifdef ENABLE_IPC
      use kernel32, only: CreateFile, GetLastError, WaitNamedPipe,
     &                    GetCurrentProcessId
     
      use, intrinsic :: iso_fortran_env, only: error_unit
      implicit none
      
      ! Get the current process ID.
      procid = GetCurrentProcessId()
      
      ! Try to open a connection to the named pipe, waiting if necessary.
      do
          ! HANDLE WINAPI CreateFile(
          !   _In_     LPCTSTR               lpFileName,
          !   _In_     DWORD                 dwDesiredAccess,
          !   _In_     DWORD                 dwShareMode,
          !   _In_opt_ LPSECURITY_ATTRIBUTES lpSecurityAttributes,
          !   _In_     DWORD                 dwCreationDisposition,
          !   _In_     DWORD                 dwFlagsAndAttributes,
          !   _In_opt_ HANDLE                hTemplateFile
          ! );
          hPipe = CreateFile(lpszPipe, GENERIC_WRITE, 0, NULL,
     &                        OPEN_EXISTING, 0, NULL)
          
          if (hPipe .ne. INVALID_HANDLE_VALUE) then
              exit
          end if
          
          if (GetLastError() .ne. ERROR_PIPE_BUSY) then
              write(error_unit, '(a, i0.3)')
     &          'Could not open connection to IPC server. Error code ',
     &          GetLastError()
              return
          end if
          
          ! Wait until pipe is available, or timeout after 3 seconds.
          if (WaitNamedPipe(lpszPipe, 3000) .eq. 0) then
              write(error_unit,'(a)') 'IPC server connection timed out.'
              return
          end if
      end do
      
      connected = .true.
#endif
      
      end subroutine ipc_connect
      
      
      subroutine ipc_disconnect()
C***********************************************************************
C                 IPC_DISCONNECT
C
C        PURPOSE: Close handle to named pipe established in IPC_CONNECT.
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    April 16, 2018
C
C        CALLED FROM:   MAIN
C***********************************************************************
#ifdef ENABLE_IPC
      use kernel32, only: CloseHandle
      implicit none
      
      integer :: rc = 0
      rc = CloseHandle(hPipe)
#endif
      
      end subroutine ipc_disconnect

      
      subroutine ipc_send_errmsg(pathwy, errtyp, errcod, lineno, modnam,
     &                           errmg1, errmg2)
C***********************************************************************
C                 IPC_SEND_ERRMSG
C
C        PURPOSE: Construct and send a message with error information.
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    April 16, 2018
C
C        INPUTS:  Error Details
C
C        CALLED FROM:   ERRHDL
C***********************************************************************
#ifdef ENABLE_IPC
      implicit none
      
      character(2),  intent(in) :: pathwy
      character(1),  intent(in) :: errtyp
      character(3),  intent(in) :: errcod
      integer(4),    intent(in) :: lineno
      character(12), intent(in) :: modnam
      character(50), intent(in) :: errmg1
      character(12), intent(in) :: errmg2
      type(errmsg_buffer_t) :: msg
      
      ! Construct the message
      msg%cbsize = sizeof(msg)
      msg%procid = procid
      msg%pathwy = pathwy
      msg%errtyp = errtyp
      msg%errcod = errcod
      msg%lineno = lineno
      msg%modnam = modnam
      msg%errmg1 = errmg1
      msg%errmg2 = errmg2
      
      ! Send the message
      call ipc_send(loc(msg), msg%cbsize)
#endif
      
      end subroutine ipc_send_errmsg
      
      
      subroutine ipc_send_tothrs(tothrs)
C***********************************************************************
C                 IPC_SEND_TOTHRS
C
C        PURPOSE: Construct and send a message with hours processed.
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    April 16, 2018
C
C        INPUTS:  Hours Processed
C
C        CALLED FROM:   HRLOOP
C***********************************************************************
#ifdef ENABLE_IPC
      implicit none
      
      integer(4), intent(in) :: tothrs
      type(tothrs_buffer_t) :: msg
      
      ! Construct the message
      msg%cbsize = sizeof(msg)
      msg%procid = procid
      msg%tothrs = tothrs
      
      ! Send the message
      call ipc_send(loc(msg), msg%cbsize)
#endif
      
      end subroutine ipc_send_tothrs
      
      
      subroutine ipc_send(lpBuffer, cbToWrite)
C***********************************************************************
C                 IPC_SEND
C
C        PURPOSE: Sends message data to IPC server via named pipe
C                 "\\\\.\\pipe\\AERMODStatusPipe". Uses Win32 API 
C                 declarations provided by Intel Fortran.
C
C        PROGRAMMER: John Buonagurio, Exponent
C
C        DATE:    April 16, 2018
C
C        INPUTS:  Progress Value
C
C        CALLED FROM:   HRLOOP
C***********************************************************************
#ifdef ENABLE_IPC
      use kernel32, only: WriteFile, GetLastError, CloseHandle
      use, intrinsic :: iso_fortran_env, only: error_unit
      implicit none
      
      integer(LPCVOID), intent(in) :: lpBuffer
      integer(DWORD), intent(in) :: cbToWrite
      integer(LPDWORD) :: cbWritten
      integer :: rc = 0
      
      if (connected .eq. .false.) then
          return
      end if
      
      ! Connection successful; send message in synchronous mode.
      ! BOOL WINAPI WriteFile(
      !   _In_        HANDLE       hFile,
      !   _In_        LPCVOID      lpBuffer,
      !   _In_        DWORD        nNumberOfBytesToWrite,
      !   _Out_opt_   LPDWORD      lpNumberOfBytesWritten,
      !   _Inout_opt_ LPOVERLAPPED lpOverlapped
      ! );
      
      rc = WriteFile(hPipe, lpBuffer, cbToWrite,
     &               loc(cbWritten), NULL)

      ! Stop sending messages after a synchronous I/O failure. 
      if (rc .eq. 0) then
          write(error_unit, '(a, i0.3)')
     &      'Failed to send message to IPC server. Error code ',
     &      GetLastError()
          connected = .false.
          return
      end if
#endif

      end subroutine ipc_send
      
      end module ipc_client
