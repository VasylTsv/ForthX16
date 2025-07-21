; Foenix F256 MicroKernel api.asm reimplementation for ACME

; Original copyright and license message follows:
;; This file is part of the TinyCore MicroKernel for the Foenix F256.
;; Copyright 2022, 2023 Jessie Oberreuter <Gadget@HackwrenchLabs.com>.
;; SPDX-License-Identifier: GPL-3.0-only
;
;; As with the Linux Kernel Exception to the GPL3, programs
;; built to run on the MicroKernel are expected to include
;; this file.  Doing so does not affect their license status.

; The following reimplementation allows to use a symbol names very similar to TASS64 code and
; add MicroKernel calls to ACME easily.
; Basically, where TASS64 would use, say, kernel.args.events.dest ACME code would have
; kernel_args_events_dest. A little bit more compex for use space structures, instead
; of myEvent.type the syntax is myEvent+off_event_type.
; Note that the syntax of this file is intentionally very simple, so it might work with other
; assemblers as is.

; Kernel Calls
; Populate the kernel_arg_* variables appropriately, and
; then JSR to one of the vectors below:

kernel_NextEvent             = $ff00 ; Copy the next event into user-space.
kernel_ReadData              = $ff04 ; Copy primary bulk event data into user-space
kernel_ReadExt               = $ff08 ; Copy secondary bolk event data into user-space
kernel_Yield                 = $ff0c ; Give unused time to the kernel.
kernel_Putch                 = $ff00 ; deprecated
kernel_RunBlock              = $ff14 ; Chain to resident program by block ID.
kernel_RunNamed              = $ff18 ; Chain to resident program by name.
kernel_reserved              = $ff1c ; reserved

kernel_BlockDevice_List      = $ff20 ; Returns a bit-set of available block-accessible devices.
kernel_BlockDevice_GetName   = $ff24 ; Gets the hardware level name of the given block device or media.
kernel_BlockDevice_GetSize   = $ff28 ; Get the number of raw sectors (48 bits) for the given device
kernel_BlockDevice_Read      = $ff2c ; Read a raw sector (48 bit LBA)
kernel_BlockDevice_Write     = $ff30 ; Write a raw sector (48 bit LBA)
kernel_BlockDevice_Format    = $ff34 ; Perform a low-level format if the media support it.
kernel_BlockDevice_Export    = $ff38 ; Update the FileSystem table with the partition table (if present).

kernel_FileSystem_List       = $ff3c ; Returns a bit-set of available logical devices.
kernel_FileSystem_GetSize    = $ff40 ; Get the size of the partition or logical device in sectors.
kernel_FileSystem_MkFS       = $ff44 ; Creates a new file-system on the logical device.
kernel_FileSystem_CheckFS    = $ff48 ; Checks the file-system for errors and corrects them.
kernel_FileSystem_Mount      = $ff4c ; Mark the file-system as available for File and Directory operations.
kernel_FileSystem_Unmount    = $ff50 ; Mark the file-system as unavailable for File and Directory operations.
kernel_FileSystem_ReadBlock  = $ff54 ; Read a partition-local raw sector on an unmounted device.
kernel_FileSystem_WriteBlock = $ff58 ; Write a partition-local raw sector on an unmounted device.

kernel_File_Open       		 = $ff5c ; Open the given file for read, create, or append.
kernel_File_Read       		 = $ff60 ; Request bytes from a file opened for reading.
kernel_File_Write      		 = $ff64 ; Write bytes to a file opened for create or append.
kernel_File_Close      		 = $ff68 ; Close an open file.
kernel_File_Rename     		 = $ff6c ; Rename a closed file.
kernel_File_Delete     		 = $ff70 ; Delete a closed file.
kernel_File_Seek       		 = $ff74 ; Seek to a specific position in a file.

kernel_Directory_Open        = $ff78 ; Open a directory for reading.
kernel_Directory_Read        = $ff7c ; Read a directory entry; may also return VOLUME and FREE events.
kernel_Directory_Close       = $ff80 ; Close a directory once finished reading.
kernel_Directory_MkDir       = $ff84 ; Create a directory
kernel_Directory_RmDir       = $ff88 ; Delete a directory

                                     ; call gate
                                     ; These are changing!
kernel_Net_GetIP             = $ff90 ; Get the local IP address.
kernel_Net_SetIP             = $ff94 ; Set the local IP address.
kernel_Net_GetDNS            = $ff98 ; Get the configured DNS IP address.
kernel_Net_SetDNS            = $ff9c ; Set the configured DNS IP address.
kernel_Net_SendICMP          = $ffa0
kernel_Net_Match             = $ffa4
					         
kernel_Net_UDP_Init          = $ffa8
kernel_Net_UDP_Send          = $ffac
kernel_Net_UDP_Recv          = $ffb0

kernel_Net_TCP_Open          = $ffb4
kernel_Net_TCP_Accept        = $ffb8
kernel_Net_TCP_Reject        = $ffbc
kernel_Net_TCP_Send          = $ffc0
kernel_Net_TCP_Recv          = $ffc4
kernel_Net_TCP_Close         = $ffc8


Display_Reset                = $ffcc ; Re-init the display
Display_GetSize              = $ffd0 ; Returns rows/cols in kernel args.
Display_DrawRow              = $ffd4 ; Draw text/color buffers left-to-right
Display_DrawColumn           = $ffd8 ; Draw text/color buffers top-to-bottom

Clock_GetTime                = $ffdc
Clock_SetTime                = $ffe0
                                     ; 65816 vectors
Clock_SetTimer               = $fff0

; Kernel Call Arguments
; Populate the structure before JSRing to the associated vector.

kernel_args = $f0
kernel_args_events    = kernel_args    ; The GetNextEvent dest address is globally reserved.
kernel_args_run       = kernel_args+3
kernel_args_recv      = kernel_args+3
kernel_args_fs        = kernel_args+3
kernel_args_file      = kernel_args+3
kernel_args_directory = kernel_args+3
kernel_args_display   = kernel_args+3
kernel_args_net       = kernel_args+3
kernel_args_config    = kernel_args+3
kernel_args_timer     = kernel_args+3

kernel_args_ext         = $f8
kernel_args_extlen      = $fa
kernel_args_buf         = $fb
kernel_args_buflen      = $fd
kernel_args_ptr         = $fe

; Event calls
kernel_args_events_dest    = kernel_args_events   ; GetNextEvent copies event data here
kernel_args_events_pending = kernel_args_events+2 ; Negative count of pending events

; Generic recv
kernel_args_recv_buf = kernel_args_buf
kernel_args_recv_buflen = kernel_args_buflen

; Run Calls
kernel_args_run_block_id = kernel_args_run

; FileSystem Calls
kernel_args_fs_format_drive     = kernel_args_fs
kernel_args_fs_format_cookie    = kernel_args_fs+1
kernel_args_fs_format_label     = kernel_args_buf
kernel_args_fs_format_label_len = kernel_args_buflen
kernel_args_fs_mkfs_drive     = kernel_args_fs
kernel_args_fs_mkfs_cookie    = kernel_args_fs+1
kernel_args_fs_mkfs_label     = kernel_args_buf
kernel_args_fs_mkfs_label_len = kernel_args_buflen
    
; File Calls
kernel_args_file_open_drive     = kernel_args_file
kernel_args_file_open_cookie    = kernel_args_file+1
kernel_args_file_open_fname     = kernel_args_buf
kernel_args_file_open_fname_len = kernel_args_buflen
kernel_args_file_open_mode      = kernel_args_file+2
kernel_args_file_open_READ      = 0
kernel_args_file_open_WRITE     = 1
kernel_args_file_open_END       = 2

kernel_args_file_read_stream    = kernel_args_file
kernel_args_file_read_buflen    = kernel_args_file+1

kernel_args_file_write_stream   = kernel_args_file
kernel_args_file_write_buf      = kernel_args_buf
kernel_args_file_write_buflen   = kernel_args_buflen

kernel_args_file_seek_stream    = kernel_args_file
kernel_args_file_seek_position  = kernel_args_file+1

kernel_args_file_close_stream   = kernel_args_file

kernel_args_file_rename_drive   = kernel_args_file
kernel_args_file_rename_cookie  = kernel_args_file+1
kernel_args_file_rename_old     = kernel_args_buf
kernel_args_file_rename_old_len = kernel_args_buflen
kernel_args_file_rename_new     = kernel_args_ext
kernel_args_file_rename_new_len = kernel_args_extlen

kernel_args_file_delete_drive   = kernel_args_file
kernel_args_file_delete_cookie  = kernel_args_file+1
kernel_args_file_delete_fname   = kernel_args_buf
kernel_args_file_delete_fname_len=kernel_args_buflen

; Directory Calls
kernel_args_directory_open_drive    = kernel_args_directory
kernel_args_directory_open_cookie   = kernel_args_directory+1
kernel_args_directory_open_path     = kernel_args_buf
kernel_args_directory_open_path_len = kernel_args_buflen

kernel_args_directory_stream = kernel_args_directory
kernel_args_directory_buflen = kernel_args_directory+1

kernel_args_directory_close_stream = kernel_args_directory

kernel_args_directory_mkdir_drive    = kernel_args_directory
kernel_args_directory_mkdir_cookie   = kernel_args_directory+1
kernel_args_directory_mkdir_path     = kernel_args_buf
kernel_args_directory_mkdir_path_len = kernel_args_buflen

kernel_args_directory_rmdir_drive    = kernel_args_directory
kernel_args_directory_rmdir_cookie   = kernel_args_directory+1
kernel_args_directory_rmdir_path     = kernel_args_buf
kernel_args_directory_rmdir_path_len = kernel_args_buflen

; Drawing Calls
kernel_args_display_x      = kernel_args_display ; coordinate or size
kernel_args_display_y      = kernel_args_display+1 ; coordinate or size
kernel_args_display_text   = kernel_args_buf       ; text
kernel_args_display_color  = kernel_args_ext       ; color
kernel_args_display_buf    = kernel_args_buf       ; deprecated
kernel_args_display_buf2   = kernel_args_ext       ; deprecated
kernel_args_display_buflen = kernel_args_buflen

; Net calls
kernel_args_net_socket = kernel_args_buf
; Init
kernel_args_net_src_port = kernel_args_net
kernel_args_net_dest_port = kernel_args_net+2
kernel_args_net_dest_ip = kernel_args_net+4
; Send/Recv
kernel_args_net_accepted = kernel_args_net
kernel_args_net_buf = kernel_args_ext
kernel_args_net_buflen = kernel_args_extlen

kernel_args_timer_units    = kernel_args_timer
kernel_args_timer_FRAMES   = 0
kernel_args_timer_SECONDS  = 1
kernel_args_timer_QUERY    = 128
kernel_args_timer_absolute = kernel_args_timer+1
kernel_args_timer_cookie   = kernel_args_timer+2
                                      
off_kernel_time_t_century = 0
off_kernel_time_t_year    = 1
off_kernel_time_t_month   = 2
off_kernel_time_t_day     = 3
off_kernel_time_t_hours   = 4
off_kernel_time_t_minutes = 5
off_kernel_time_t_seconds = 6
off_kernel_time_t_centis  = 7
off_kernel_time_t_size    = 8

; Events
; The vast majority of kernel operations communicate with userland
; by sending events; the data contained in the various events are
; described following the event list.

kernel_event_JOYSTICK          = $04 ; Game Controller changes.
kernel_event_DEVICE            = $06 ; Device added/removed.

kernel_event_key_PRESSED       = $08 ; Key pressed
kernel_event_key_RELEASED      = $0a ; Key released.

kernel_event_mouse_DELTA       = $0c ; Regular mouse move and button state
kernel_event_mouse_CLICKS      = $0e ; Click counts

kernel_event_block_NAME        = $10
kernel_event_block_SIZE        = $12
kernel_event_block_DATA        = $14 ; The read request has succeeded.
kernel_event_block_WROTE       = $16 ; The write request has completed.
kernel_event_block_FORMATTED   = $18 ; The low-level format has completed.
kernel_event_block_ERROR       = $1a

kernel_event_fs_SIZE           = $1c
kernel_event_fs_CREATED        = $1e
kernel_event_fs_CHECKED        = $20
kernel_event_fs_DATA           = $22 ; The read request has succeeded.
kernel_event_fs_WROTE          = $24 ; The write request has completed.
kernel_event_fs_ERROR          = $26

kernel_event_file_NOT_FOUND    = $28 ; The file file was not found.
kernel_event_file_OPENED       = $2a ; The file was successfully opened.
kernel_event_file_DATA         = $2c ; The read request has succeeded.
kernel_event_file_WROTE        = $2e ; The write request has completed.
kernel_event_file_EOF          = $30 ; All file data has been read.
kernel_event_file_CLOSED       = $32 ; The close request has completed.
kernel_event_file_RENAMED      = $34 ; The rename request has completed.
kernel_event_file_DELETED      = $36 ; The delete request has completed.
kernel_event_file_ERROR        = $38 ; An error occured; close the file if opened.
kernel_event_file_SEEK         = $3a ; The seek request has completed.

kernel_event_directory_OPENED  = $3c ; The directory open request succeeded.
kernel_event_directory_VOLUME  = $3e ; A volume record was found.
kernel_event_directory_FILE    = $40 ; A file record was found.
kernel_event_directory_FREE    = $42 ; A file-system free-space record was found.
kernel_event_directory_EOF     = $44 ; All data has been read.
kernel_event_directory_CLOSED  = $46 ; The directory file has been closed.
kernel_event_directory_ERROR   = $48 ; An error occured; user should close.
kernel_event_directory_CREATED = $4a ; The directory has been created.
kernel_event_directory_DELETED = $4c ; The directory has been deleted.

kernel_event_net_TCP           = $4e
kernel_event_net_UDP           = $50

kernel_event_timer_EXPIRED     = $52

kernel_event_clock_TICK        = $54

kernel_event_irq_IRQ           = $56

off_event_type = 0 ; Enum above
off_event_buf  = 1 ; page id or zero
off_event_ext  = 2 ; page id or zero

off_event_key       = 3
off_event_mouse     = 3
off_event_joystick  = 3
off_event_udp       = 3
off_event_tcp       = 3
off_event_file      = 3
off_event_directory = 3
off_event_timer     = 3
off_event_irq       = 3


; Data in keyboard events
off_event_key_keyboard = off_event_key   ; Keyboard ID
off_event_key_raw      = off_event_key+1 ; Raw key ID
off_event_key_ascii    = off_event_key+2 ; ASCII value
off_event_key_flags    = off_event_key+3 ; Flags (META)
event_key_META         = $80             ; Meta key; no associated ASCII value.
            
; Data in mouse events
off_event_mouse_delta_x       = off_event_mouse
off_event_mouse_delta_y       = off_event_mouse+1
off_event_mouse_delta_z       = off_event_mouse+2
off_event_mouse_delta_buttons = off_event_mouse+3

off_event_mouse_inner  = off_event_mouse
off_event_mouse_middle = off_event_mouse+1
off_event_mouse_outer  = off_event_mouse+2


off_event_joystick_joy0 = off_event_joystick
off_event_joystick_joy1 = off_event_joystick+1

; Data in file events:
off_event_file_stream = off_event_file
off_event_file_cookie = off_event_file+1
; ext contains disk id
off_event_file_data_requested = off_event_file+2 ; Requested number of bytes to read
off_event_file_data_read      = off_event_file+3 ; Number of bytes actually read
; ext contains disk id
off_event_file_wrote_requested = off_event_file+2 ; Requested number of bytes to read
off_event_file_wrote_wrote     = off_event_file+3 ; Number of bytes actually read

; Data in directory events:
off_event_directory_stream = off_event_directory
off_event_directory_cookie = off_event_directory+1
; ext contains disk id
off_event_directory_volume_len   = off_event_directory+2 ; Length of volname (in buf)
off_event_directory_volume_flags = off_event_directory+3 ; block size, text encoding
; ext contains byte count and modified date
off_event_directory_file_len   = off_event_directory+2
off_event_directory_file_flags = off_event_directory+3 ; block scale, text encoding, approx size
; ext contains byte count and modified date
off_event_directory_free_flags = off_event_directory+2 ; block scale, text encoding, approx size
			
;dir_ext_t   .struct     ; Extended information; more to follow.
;free        .fill   6   ; blocks used/free
;            .ends

; Data in net events (major changes coming)
off_event_udp_token = off_event_udp ; TODO: break out into fields

off_event_tcp_len = off_event_tcp ; Raw packet length.

off_event_timer_value  = off_event_timer
off_event_timer_cookie = off_event_timer+1

off_event_irq_group  = off_event_irq 
off_event_irq_bitval = off_event_irq+1
