import { createContext, useContext } from 'react'

interface BoardContextValue {
  sendCommand: (command: string, data?: any) => void
  currentBoardId: string | null
}

export const BoardContext = createContext<BoardContextValue>({
  sendCommand: () => {},
  currentBoardId: null,
})

export const useBoardContext = () => useContext(BoardContext)
