package object gui:
  /**
    * The Game GUI is currently fixed-size with static layout of
    * the components. The fixed-size and static characteristics
    * make it easier to deal with the geometry of the triangle tiles
    * in particular.
    * 
    * The GUI window has a fixed size of width 1205 and height 700.
    * There are 4 main components in the game GUI, and their layout
    * is divided as follow:

      +-------------------------------------+-----------------------+-------------------------------------+
      | LEFT PANEL                          | PANEL SEPARATOR       | RIGHT PANEL                         |
      |                                     |                       |                                     |
      | GameBoard here                      |                       | WaitingBoard here                   |
      |                                     |                       |                                     |
      | Panel: W=600 - H=600                | Panel: W=5 - H=600    | Panel: W=600 - H=600                |
      |                                     |                       |                                     |
      | Board: W=500 - H=500 (inside panel) |                       | Board: W=500 - H=500 (inside panel) |
      |                                     |                       |                                     |
      +-------------------------------------+-----------------------+-------------------------------------+
      |                                      BOTTOM PANEL                                                 |
      |                                      Control Buttons here                                         |
      |                                      Panel: W=1205 - H=100                                        |
      +-------------------------------------+-----------------------+-------------------------------------+


    */

  // Define constants for sizes and positions
  val windowWidth = 1205
  val windowHeight = 700
  val boardPanelSize = 600
  val boardHexagonSize = 500
  val centerX = boardPanelSize / 2
  val centerY = centerX

end gui