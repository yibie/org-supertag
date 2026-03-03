import { create } from 'zustand'
import {
  BoardNode,
  BoardEdge,
  BoardGroup,
  BoardViewport,
  BoardInfo,
  AvailableNode,
  BoardData,
} from './types'

interface BoardState {
  // Current board
  currentBoardId: string | null
  boardTitle: string
  nodes: BoardNode[]
  edges: BoardEdge[]
  groups: BoardGroup[]
  viewport: BoardViewport

  // Board list
  boards: BoardInfo[]

  // Available nodes for palette
  availableNodes: AvailableNode[]

  // Follow mode
  followedNodeId: string | null

  // Actions
  setBoards: (boards: BoardInfo[]) => void
  setBoardData: (data: BoardData) => void
  setAvailableNodes: (nodes: AvailableNode[]) => void
  setFollowedNode: (id: string | null) => void
  clearBoard: () => void
}

export const useBoardStore = create<BoardState>((set) => ({
  currentBoardId: null,
  boardTitle: '',
  nodes: [],
  edges: [],
  groups: [],
  viewport: { x: 0, y: 0, zoom: 1.0 },
  boards: [],
  availableNodes: [],
  followedNodeId: null,

  setBoards: (boards) => set({ boards }),

  setBoardData: (data) =>
    set({
      currentBoardId: data.boardId,
      boardTitle: data.title,
      nodes: data.nodes,
      edges: data.edges,
      groups: data.groups,
      viewport: data.viewport,
    }),

  setAvailableNodes: (nodes) => set({ availableNodes: nodes }),
  setFollowedNode: (id) => set({ followedNodeId: id }),

  clearBoard: () =>
    set({
      currentBoardId: null,
      boardTitle: '',
      nodes: [],
      edges: [],
      groups: [],
      viewport: { x: 0, y: 0, zoom: 1.0 },
    }),
}))
