export interface BoardNode {
  id: string
  title: string
  tags: string[]
  tagFields?: Record<string, Array<{ name: string; value: string }>>
  content?: string
  x: number
  y: number
  width: number
  height?: number
  collapsed: boolean
}

export interface BoardEdge {
  id: string
  from: string
  to: string
  label: string
  style: string
  color?: string
  sourceHandle: string
  targetHandle: string
  isGlobal: boolean
}

export interface BoardGroup {
  id: string
  label: string
  x: number
  y: number
  width: number
  height: number
  color: string
  nodeIds: string[]
}

export interface BoardViewport {
  x: number
  y: number
  zoom: number
}

export interface BoardInfo {
  id: string
  title: string
}

export interface BoardData {
  boardId: string
  title: string
  nodes: BoardNode[]
  edges: BoardEdge[]
  groups: BoardGroup[]
  viewport: BoardViewport
}

export interface AvailableNode {
  id: string
  title: string
  tags: string[]
  file: string
}
