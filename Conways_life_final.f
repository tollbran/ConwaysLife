
{ ------------------------------------------------------------------------------------------- }
{                                                                                             }
{ Brandon Tollan Conways_life.final.f code last updated on 17/03/21                           } 
{ 																					          }
{                                                                                             }
{ This version contains all the code needed to investigate the basic rules of conways life    }
{ in addition to investigating the phase transitions due to the rule set                      }
{                                                                                             }
{ The instructions of how to use this code are specified in the README file in same directory }
{ ------------------------------------------------------------------------------------------- }

{ ------------------------------- Global constants and variables ---------------------------- }


10 Constant Update-Timer    { Sets windows update rate - lower = faster refresh               }

variable bmp-x-size         { x dimension of bmp file                                         }

variable bmp-y-size         { y dimension of bmp file                                         }

variable bmp-size           { Total number of bmp elements = (x * y)                          }

variable bmp-address        { Stores start address of bmp file # 1                            }

variable bmp-length         { Total number of chars in bmp including header block             }

variable bmp-x-start        { Initial x position of upper left corner                         }

variable bmp-y-start        { Initial y position of upper left corner                         }

variable bmp-window-handle  { Variable to store the handle used to ID display window          }

variable offset             { Memory offset used in bmp pixel adddress examples               }

variable size               { Defines number of pixels array takes up                         }

variable width              { Defines the width of the grid array takes up                    }
    
variable neighbours         { Holds number of alive pixels around a cell                      }

variable pos                { Holds a integer to signify position along the array             }

variable pixel_loc          { Holds adress of pixel being analysed by apperture               }

variable row                { Holds the number of the row currently on by the anlayse }

variable weighting          { Holds weighting probability for a cell to be initially alive    }

variable alive_cells        { Holds number of alive cells within one generation               }

variable generation         { Holds the cycle that the simulation is on                       }

variable x_pos              { Holds the x position of the cell being checked for neighbours   }

variable y_pos				{ Holds the y position of the cell being checked for neighbours   }

variable x_app				{ The apperture's cell y position                                 }

variable y_app				{ The apperture's cell x position                                 }

variable x_adjust_app		{ The adjusted x pos if apperture cell is outside grid boundaries }

variable y_adjust_app       { The adjusted y pos if apperture cell is outside grid boundaries }

variable cycles             { Sets the number of cycles the simulation will run for           }

variable sync               { Stores the synconicty                                           }

128 bmp-x-size !                               { Set default x size of bmp in pixels          }

128 bmp-y-size !                               { Set y default size of bmp in pixels          }

bmp-x-size @ 4 / 1 max 4 *  bmp-x-size !       { Trim x-size to integer product of 4          }

bmp-x-size @ bmp-y-size @ * bmp-size !         { Find number of pixels in bmp                 }

bmp-size   @ 3 * 54 +       bmp-length !       { Find length of bmp in chars inc. header      }

100 bmp-x-start !                              { Set x position of upper left corner          }

100 bmp-y-start !                              { Set y position of upper left corner          }

: bmp-Wind-Name Z" BMP Display " ;             { Set capion of the display window # 1         }

50 weighting !                                 { Sets default weighting                       }
 
128 width !                                    { Sets default width                           }
  
width @ dup *  size !                          { Sets default size                            }

0 generation !                                 { Sets starting generation to 0                }

1000 cycles !                                 { Sets default no of cycles                    }

50 sync !                                      { Sets default syncronicity                    }

{ --------------------------------- File writing stuff ---------------------------------- } 

variable test-file-id                             { Create Variable to hold file id handle }


: make-test-file                                                                   { Create a test file to read / write to  }
  s" C:\ForthInc-Evaluation\Projects\ConwaysLife\methu.dat" r/w create-file drop     { Create the file                        } 
  test-file-id !                                                                   { Store file handle for later use        }
;

 
: open-test-file                                                                   { Open the file for read/write access    }
  s" C:\ForthInc-Evaluation\Projects\ConwaysLife\methu.dat" r/w open-file drop       { Not needed if we have just created     }
  test-file-id !                                                                   { file.                                  }
;


: close-test-file                                 { Close the file pointed to by the file  }
  test-file-id @                                  { handle.                                }
  close-file drop
; 


: test-file-size                                  { Leave size of file on top of stack as  }
  test-file-id @                                  { a double prescision integer if the     }
  file-size drop                                  { file is open.                          }
;


: write-gen-alive
  generation @ (.) test-file-id @ write-file drop   { Writes the data in the generation variable to a text doc }
  s"  "     test-file-id @ write-file drop          { Creates a blank space next to it                         }
  s"  "     test-file-id @ write-file drop          { Creates a blank space next to it                         }
  s"  "     test-file-id @ write-file drop          { Creates a blank space next to it                         }
  alive_cells @  (.) test-file-id @ write-line drop { Writes the data in the alive cells, creates a new line   }
;

{ -------------------------  Random number routine for testing ------------------------- } 

CREATE SEED  123475689 ,

: Rnd ( n -- rnd )   { Returns single random number less than n }
   SEED              { Minimal version of SwiftForth Rnd.f      }
   DUP >R            { Algorithm Rick VanNorman  rvn@forth.com  }
   @ 127773 /MOD 
   2836 * SWAP 16807 * 
   2DUP > IF - 
   ELSE - 2147483647 +  
   THEN  DUP R> !
   SWAP MOD ;

{ -------------------------------- Array commands & Variables -------------------------------- }

: make_small_array                   {  Makes the array according to the specified dimensions  }
   size ALLOCATE drop ;

make_small_array CONSTANT array      { Makes the main array used for life and stores it        }

make_small_array CONSTANT array_temp { Makes a temp array used to transfer data                }

: fill_array
   size @ 0 DO                       { Fills the initall array with a random number of 1 and 0       }
    100 RND weighting @ <			 { Check if a random number between 1-100 is less than weighting }
	IF								 { If true we set cell to 1 alive                                }
		1 array I + c!               { If false we se cell to 0 dead                                 }
	ELSE
	    0 array I + c!
	THEN
   LOOP ;

: reset_array                        { Sets all the position in main array to 0 }
   array size 0 FILL ;

: reset_array_temp                   { Sets all the position in temp array to 0 }
   array_temp size 0 FILL ;

: show_array         
  size @ 0 DO        
   I width @ mod 0=  { This goes through the array printing the contents in console }
   IF                { Prints it in rows according to the dimension of the grid     }
    cr               { Used for debugging purposes                                  }
   THEN 
    array I + c@ .
  LOOP ;

: array_!           {  Allows us to append a value to the array through an inputy (x,y) }
  width @ *         {  We write it as n x y where n is the value                        }
  +                 {  And x and y are the position (x,y)                               }
  array + c! ;

: array_temp!       {  Allows us to append a value to the temp array through an inputy (x,y) }
  width @ *         {  We write it as n x y where n is the value                             }
  +                 {  And x and y are the position (x,y)                                    }
  array_temp + c! ; 

: array_@         {  Allow us to retive the data stored in the array  }
  width @ *       {  in a position (x,y)                              }
  +
  array + c@ ;

: array_temp@     {  Allow us to retive the data stored in the temp array  }
  width @ *       {  in a position (x,y)                                   }
  +
  array_temp + c@ ;


{ --------------------------------  Life logic  -------------------------------- }

: count_neighbours 
	x_pos @ 1 - x_app !                                      { Starts at the top left corner of the 3x3 apperture            }
	y_pos @ 1 - y_app !										 { Centered on the x_pos y_pos                                   }
	3 0 DO 
		3 0 DO                                               { We cycle across each cell in the apperture Left-Right Up-Down }
		x_app @ -1 =                                         { We check if the apperture detects cell beyond the left border }
		IF                                                   { If does we loop around to cell on right border                }
			x_app @ width @ + x_adjust_app !
		ELSE
			x_app @ width @ =                                { Checks if the apperture detects cell beyong right border      }
			IF  											 { If does we loop around to cell on left border                 }
				0 x_adjust_app !
			ELSE
				x_app @ x_adjust_app !                       { If all is okay, set the apperture x pos as one to be checked  }
			THEN
		THEN
		
		y_app @ -1 =                                         { Check if apperture detects a cell above the grid              }
		IF                                                   { If true set the y pos to cell at bottom of grid               }
			y_app @ width @ + y_adjust_app !
		ELSE
			y_app @ width @ =                                { Checks if apperture detects a cell below the grid             }
			IF                                               { If true set the y pos to cell at the top of the grid          }
				0 y_adjust_app !
			ELSE
				y_app @ y_adjust_app !                       { If all is okay, set the apperture y pos as one to be checked  }
			THEN
		THEN
		
		y_adjust_app @ width @ * x_adjust_app @ array + + c@ { The state of the cell at the grid location is found           }
		1 =                                                  { If it is alive we add one to the neigbour count               }
		IF
			neighbours @ 1 + neighbours !
		THEN
		
		
		x_app @ 1 + x_app !                                  { Moves to next x pos to the right                              }
		LOOP
    y_app @ 1 + y_app !                                      { Moves to next y pos downwards                                 }
	x_app @ 3 - x_app !                                      { Moves back to the initial x pos                               }
	LOOP
;

: pixel_update 
   0 x_pos !  
   0 y_pos !                                  { Variables runs from 0 to width - 1                         }
   size @ 0 DO                                { Runs through all cell in array                             }
	x_pos @ width @ /                         { We detect if we are at the edge of the grid                }  
	1 =                                       { If this is true we reset the x pos and move down a row     }
	IF
		0 x_pos !
		y_pos @ 1 + y_pos !
	THEN
	
	100 RND sync @ <                            { Checking if a random no 0-100 less than the sync threshold }
	IF 
		0 neighbours !                        { Reset neigbour count                                       }
		array I + pixel_loc !                 { Store the position of cell in question in variable         }
		count_neighbours                  
		array I + c@ 1 =                      { If the cell in question is alive                           }
		IF                                    { Detects if the cell is alive or dead                       }
		 neighbours @ 1 - neighbours !        { Excludes itself                                            }
		 neighbours @ 3 = neighbours @ 2 = OR { Rule if there is 2 or three alive cells next               } 
		 IF
		  1 array_temp I + c!                 { Sets cell to alive                                         }
		 ELSE
		  0 array_temp I + c!                 { Sets cell to dead                                          }
		 THEN
		ELSE                                  { Else the cell in question is dead                          }
		 neighbours @ 3 =                     { Rule if there is 3 live cells next to dead revive          }
		 IF
		  1 array_temp I + c!                 { Sets cell to alive                                         }
		 ELSE
		  0 array_temp I + c!                 { Sets cell to dead                                          }                                         
		 THEN
		THEN
	THEN
	
	array I + c@ 1 =                          { Checks if cell is alive                                    }
	IF                                        { If true, adds one onto the total alive cell count          }
		alive_cells @ 1 + alive_cells !    
	THEN
	
	x_pos @ 1 + x_pos !
	
   LOOP
   ;

: data_transfer
   size @ 0 DO            { transfers data from the temp array to the main array  }
    array_temp I + c@     { this is done at the end of every gen to stop updating }
    array I + c!          { impacting the game.                                   }
   LOOP
   ;


{ --------------------------- Words to create a bmp file in memory ----------------------- }


: Make-Memory-bmp  ( x y  -- addr )        { Create 24 bit (RGB) bitmap in memory          }
  0 Locals| bmp-addr y-size x-size |
  x-size y-size * 3 * 54 +                 { Find number of bytes required for bmp file    }
  chars allocate                           { Allocate  memory = 3 x size + header in chars }
  drop to bmp-addr
  bmp-addr                                 { Set initial bmp pixels and header to zero     }
  x-size y-size * 3 * 54 + 0 fill

  { Create the 54 byte .bmp file header block }

  66 bmp-addr  0 + c!                      { Create header entries - B                     }
  77 bmp-addr  1 + c!                      { Create header entries - M                     }
  54 bmp-addr 10 + c!                      { Header length of 54 characters                } 
  40 bmp-addr 14 + c!   
   1 bmp-addr 26 + c!
  24 bmp-addr 28 + c!                      { Set bmp bit depth to 24                       }
  48 bmp-addr 34 + c!
 117 bmp-addr 35 + c!
  19 bmp-addr 38 + c!
  11 bmp-addr 39 + c!
  19 bmp-addr 42 + c!
  11 bmp-addr 43 + c!
 
  x-size y-size * 3 * 54 +                 { Store file length in header as 32 bit Dword   }
  bmp-addr 2 + !
  x-size                                   { Store bmp x dimension in header               }
  bmp-addr 18 + ! 
  y-size                                   { Store bmp y dimension in header               }
  bmp-addr 22 + ! 
  bmp-addr                                 { Leave bmp start address on stack and exit     }
  ;


{ ---------------------------------- Stand Alone Test Routines --------------------------- }


 : Setup-Test-Memory  ( -- )                       { Create bmps in memory to start with }
   bmp-x-size @ bmp-y-size @ make-memory-bmp
   bmp-address ! 
   cr ." Created Test bmp " cr
   ;


{ ----------------------------- Word to colour the pixels -----------------------------------}

: set-pixel ( adrr -- ) 
  dup dup 2 + @ + swap 54 + DO
   pos @ array + c@
   0=
   IF 
    255                                   { Red   RGB value                                }
    0                                     { Green RGB value                                }
    0                                     { Blue  RGB value                                }

   ELSE
    0                                     { Red   RGB value                                }
    0                                     { Green RGB value                                }
    0                                     { Blue  RGB value                                }

   THEN
    pos @ 1 + pos ! 
    i  tuck c!
    1+ tuck c!
    1+      c!    
    3 +LOOP ;


{ -------------------- Word to display a bmp using MS Windows API Calls -----------------  }
{                                                                                          }
{ Warning, this section contains MS Windows specific code to create and communicate with a }
{ new display window and will not automatically translate to another OS, e.g. Mac or Linux }


Function: SetDIBitsToDevice ( a b c d e f g h i j k l -- res )

: MEM-bmp ( addr -- )                    { Prints bmp starting at address to screen        }
   [OBJECTS BITMAP MAKES BM OBJECTS]
   BM bmp!
   HWND GetDC ( hDC )
   DUP >R ( hDC ) 1 1 ( x y )            { (x,y) upper right corner of bitmap              }
   BM Width @ BM Height @ 0 0 0
   BM Height @ BM Data
   BM InfoHeader DIB_RGB_COLORS SetDIBitsToDevice DROP  { Windows API calls                }   
   HWND R> ( hDC ) ReleaseDC DROP ;



{ ---------------------- bmp Display Window Class and Application ------------------------ }
{                                                                                          }
{ Warning, this section contains MS Windows specific code to create and communicate with a }
{ new display window and will not automatically translate to another OS, e.g. Mac or Linux }


0 VALUE bmp-hApp            { Variable to hold handle for default bmp display window     }


: bmp-Classname Z" Show-bmp" ;      { Classname for the bmp output class          }


: bmp-End-App ( -- res )
   'MAIN @ [ HERE CODE> ] LITERAL < IF ( not an application yet )
      0 TO bmp-hApp
   ELSE ( is an application )
      0 PostQuitMessage DROP
   THEN 0 ;


[SWITCH bmp-App-Messages DEFWINPROC ( msg -- res ) WM_DESTROY RUNS bmp-End-App SWITCH]


:NONAME ( -- res ) MSG LOWORD bmp-App-Messages ; 4 CB: bmp-APP-WNDPROC { Link window messages to process }


: bmp-APP-CLASS ( -- )
      0  CS_OWNDC   OR                  \ Allocates unique device context for each window in class
         CS_HREDRAW OR                  \ Window to be redrawn if movement / size changes width
         CS_VREDRAW OR                  \ Window to be redrawn if movement / size changes height
      bmp-APP-WNDPROC                   \ wndproc
      0                                 \ class extra
      0                                 \ window extra
      HINST                             \ hinstance
      HINST 101  LoadIcon 
   \   NULL IDC_ARROW LoadCursor        \ Default Arrow Cursor
      NULL IDC_CROSS LoadCursor         \ Cross cursor
      WHITE_BRUSH GetStockObject        \
      0                                 \ no menu
      bmp-Classname                     \ class name
   DefineClass DROP
  ;


: bmp-window-shutdown     { Close bmp display window and unregister classes on shutdown   }               
   bmp-hApp IF 
   bmp-hApp WM_CLOSE 0 0 SendMessage DROP
   THEN
   bmp-Classname HINST UnregisterClass DROP
  ;


bmp-APP-CLASS                   { Call class for displaying bmp's in a child window     }

13 IMPORT: StretchDIBits

11 IMPORT: SetDIBitsToDevice 


{ ----------------------------- bmp Window Output Routines -------------------------------- }
{                                                                                           }
{  Create a new "copy" or "stretch" window, save its handle, and then output a .bmp from    }
{  memory to the window in "copy" mode or "stretch" mode.  You will need to write your own  }
{  data to the .bmp between each display cycle to give a real time view of your simulation. }



: New-bmp-Window-Stretch  ( -- res )         \ Window class for "stretch" display 
   0                                         \ exended style
   bmp-Classname                             \ class name
   s" BMP Window " pad zplace                \ window title - including bmp number
   1  (.) pad zappend pad
   WS_OVERLAPPEDWINDOW                       \ window style
   bmp-x-start @ bmp-y-start @               \ x   y Window position
   bmp-x-size @ 250 max 10 + 
   bmp-y-size @ 250 max 49 +                 \ cx cy Window size, min start size 250x250
   0                                         \ parent window
   0                                         \ menu
   HINST                                     \ instance handle
   0                                         \ creation parameters
   CreateWindowEx 
   DUP 0= ABORT" create window failed" 
   DUP 1 ShowWindow DROP
   DUP UpdateWindow DROP 
   ;

   
: bmp-to-screen-stretch  ( n addr -- )    { Stretch bmp at addr to window n             }
  0 0 0 
  Locals| bmp-win-hWnd bmp-win-x bmp-win-y bmp-address |
  bmp-window-handle @
  dup to bmp-win-hWnd                     { Handle of device context we want to draw in }
  PAD GetClientRect DROP                  { Get x , y size of window we draw to         }
  PAD @RECT 
  to bmp-win-y to bmp-win-x
  drop drop                             
  bmp-win-hWnd GetDC                      { Get device context of window we draw to     }
  2 2                                     { x , y of upper-left corner of dest. rect.   }   
  bmp-win-x 4 - bmp-win-y 4 -             { width, height of destination rectangle      }
  0 0                                     { x , y of upper-left corner of source rect.  }
  bmp-address 18 + @                      { Width of source rectangle                   }
  bmp-address 22 + @                      { Height of source rectangle                  }
  bmp-address dup 54 + swap 14 +          { address of bitmap bits, bitmap header       }
  0                                       { usage                                       }
  13369376                                { raster operation code                       }  
  StretchDIBits drop
  ;


{ ----------------------------- Demonstration Routines -------------------------------- }


: go-stretch                          { Draw bmp to screen at variable pixel size       }
  cr ." Starting looped stretch to window test " 
  cr cr
  New-bmp-Window-stretch              { Create new "stretch" window                     }
  bmp-window-handle !                 { Store window handle                             }
  make-test-file
  fill_array                          { Fills the array with a random set of 1-0s       }
  cycles @ 0 DO
  
	{  All code which runs the program goes here  }
  
	0 pos !
	0 alive_cells !
	bmp-address @ set-pixel             { Reads the array and puts associated data in bnp }
	bmp-address @ bmp-to-screen-stretch { Stretch .bmp to display window                  }
	pixel_update                        { Enforces life rules and output to temp array    }
	data_transfer                       { Transfers data from temp array to main one      }
	write-gen-alive
	generation @ 1 + generation !       { Moves onto next generation                      }
	1 ms                                { Delay for viewing ease, reduce for higher speed }

  LOOP
  close-test-file
  cr ." Ending looped stretch to window test " 
  cr cr
  ;


{ --------------------------------  Testing procedures  -------------------------------- }

{ If you desire to test these seeds, write one down at the bottom before go-strech word  }
{ and make sure to blank out the fill_array word in go-strech, this stops the auto       }
{ updating of cells.                                                                     }

: stable_block_one  {  This generates the most basic stable seed }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size ! 
  1 3 3 array_!
  1 3 4 array_!
  1 4 4 array_!
  ;



: unstable_block_one  { This generates a basic unstable seed }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size !
  1 2 2 array_!
  1 1 3 array_!
  1 2 4 array_!
  1 3 4 array_!
  ;

: blinker_one { This generates a stable seed, which oscilates with a period of 2 }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size !
  1 2 2 array_!
  1 2 3 array_!
  1 2 4 array_!
  ;


: stable_block_two { This generates another stable seed, but on a larger grid }
  16 width !
  width @ dup *  size !
  16 bmp-x-size !                             
  16 bmp-y-size !
  1 6 6 array_!
  1 6 7 array_!
  1 6 8 array_!
  1 6 9 array_!
  1 6 10 array_!
  ;

: stable_block_three  {  This generates the third stable seed }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size !
  1 2 2 array_!
  1 1 3 array_!
  1 2 4 array_!
  1 3 4 array_!
  1 3 3 array_!
  ;


: stable_block_four  {  This generates the forth stable seed }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size !
  1 2 2 array_!
  1 1 3 array_!
  1 2 4 array_!
  1 3 3 array_!
  ;

: stable_block_five  { This generates the fifth stable test seed }
  8 width !
  width @ dup *  size !
  8 bmp-x-size !                             
  8 bmp-y-size !
  1 2 2 array_!
  1 3 2 array_!
  1 2 3 array_!
  1 5 3 array_!
  1 5 4 array_!
  1 4 4 array_!
  ;


: stable_block_six  {  This generates the sixth stable seed }
  16 width !
  width @ dup *  size !
  16 bmp-x-size !                             
  16 bmp-y-size ! 
  1 3 3 array_!
  1 3 4 array_!
  1 4 3 array_!
  1 5 4 array_!
  1 6 5 array_!
  1 7 6 array_!
  ;

: glider  { This generatesa glider which moves across the screen indefinantly }
  32 width !
  width @ dup *  size !
  32 bmp-x-size !                             
  32 bmp-y-size ! 
  1 10 20 array_!
  1 10 21 array_!
  1 10 22 array_!
  1 11 22 array_!
  1 12 21 array_!
  ;

: methu_one { This generates a Methuselah seed }
  256 width !
  width @ dup *  size !
  256 bmp-x-size !                             
  256 bmp-y-size ! 
  1 200 200 array_!
  1 201 200 array_!
  1 202 200 array_!
  1 199 201 array_!
  1 199 202 array_!
  ;
  
{ --------------------------------  Running Program  -------------------------------- }

{ Alter these variables to run }

1000 cycles !  { Changes to specify no of cycles before simulation ends           }
20 sync !      { Change to alter the syncronicity of the system: 0-100            }
50 weighting !  { Change to alter the initial chance of a cell being alive: 0-100  }
128 width !     { Change to alter the initial width of the grid                    }


reset_array
reset_array_temp

{ methu_one }         { Write your test seed here }

Setup-Test-Memory { Initially sets up the bmp to be processed  }
go-stretch        {  Runs the program  }
reset_array
reset_array_temp
