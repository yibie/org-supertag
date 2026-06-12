import { createContext, useContext } from 'react'

interface BoardContextValue {
  sendCommand: (command: string, data?: any) => void
  currentBoardId: string | null
  searchQuery: string
  onAutoLayout?: () => void
}

export const BoardContext = createContext<BoardContextValue>({
  sendCommand: () => {},
  currentBoardId: null,
  searchQuery: '',
  onAutoLayout: undefined,
})

export const useBoardContext = () => useContext(BoardContext)
