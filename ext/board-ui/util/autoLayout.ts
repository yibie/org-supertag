/**
 * Auto-layout for board nodes using a layered (top-down) approach.
 * Uses a simple Sugiyama-style layout: topological sort → assign layers → position.
 * No external dependency required.
 */
import type { BoardNode, BoardEdge } from '../store/types'

interface LayoutResult {
  id: string
  x: number
  y: number
}

const NODE_WIDTH = 180
const NODE_HEIGHT = 120
const HORIZONTAL_GAP = 60
const VERTICAL_GAP = 80
const START_X = 40
const START_Y = 40

export function applyAutoLayout(
  nodes: BoardNode[],
  edges: BoardEdge[]
): LayoutResult[] {
  if (nodes.length === 0) return []

  const nodeIds = new Set(nodes.map((n) => n.id))

  // Build adjacency
  const children = new Map<string, string[]>()
  const parents = new Map<string, string[]>()
  const inDegree = new Map<string, number>()

  // Initialize
  nodes.forEach((n) => {
    children.set(n.id, [])
    parents.set(n.id, [])
    inDegree.set(n.id, edges.filter((e) => e.to === n.id && nodeIds.has(e.from)).length)
  })

  // Fill adjacency from edges
  edges.forEach((e) => {
    if (!nodeIds.has(e.from) || !nodeIds.has(e.to)) return
    children.get(e.from)!.push(e.to)
    parents.get(e.to)!.push(e.from)
  })

  // Topological sort to assign layers (longest-path algorithm for layered layout)
  const layer = new Map<string, number>()
  const visited = new Set<string>()

  function assignLayer(nodeId: string): number {
    if (layer.has(nodeId)) return layer.get(nodeId)!
    if (visited.has(nodeId)) return 0 // cycle, put at 0
    visited.add(nodeId)

    const parentLayers = (parents.get(nodeId) || []).map((p) => assignLayer(p))
    const maxParentLayer = parentLayers.length > 0 ? Math.max(...parentLayers) : -1
    const l = maxParentLayer + 1
    layer.set(nodeId, l)
    return l
  }

  nodes.forEach((n) => assignLayer(n.id))

  // Group nodes by layer
  const layerGroups = new Map<number, string[]>()
  nodes.forEach((n) => {
    const l = layer.get(n.id) || 0
    if (!layerGroups.has(l)) layerGroups.set(l, [])
    layerGroups.get(l)!.push(n.id)
  })

  // Sort layers
  const sortedLayers = Array.from(layerGroups.keys()).sort((a, b) => a - b)

  // Assign positions: within a layer, try to minimize edge crossings by
  // sorting by barycenter of connected nodes in previous layer
  const nodePositions = new Map<string, { x: number; y: number }>()

  sortedLayers.forEach((l, layerIndex) => {
    const nodeIdsInLayer = layerGroups.get(l)!

    // Sort by barycenter of parents (to reduce edge crossings)
    if (layerIndex > 0) {
      const prevLayer = sortedLayers[layerIndex - 1]
      const prevPositions = new Map(
        layerGroups.get(prevLayer)!.map((id, i) => [id, i])
      )

      nodeIdsInLayer.sort((a, b) => {
        const aParents = parents.get(a) || []
        const bParents = parents.get(b) || []
        const aBary = aParents.length > 0
          ? aParents.reduce((sum, p) => sum + (prevPositions.get(p) ?? 0), 0) / aParents.length
          : Infinity
        const bBary = bParents.length > 0
          ? bParents.reduce((sum, p) => sum + (prevPositions.get(p) ?? 0), 0) / bParents.length
          : Infinity
        return aBary - bBary
      })
    }

    const totalWidth = nodeIdsInLayer.length * (NODE_WIDTH + HORIZONTAL_GAP) - HORIZONTAL_GAP
    const startX = START_X

    nodeIdsInLayer.forEach((nodeId, colIndex) => {
      nodePositions.set(nodeId, {
        x: startX + colIndex * (NODE_WIDTH + HORIZONTAL_GAP),
        y: START_Y + layerIndex * (NODE_HEIGHT + VERTICAL_GAP),
      })
    })
  })

  // Return only nodes that actually moved
  return nodes
    .filter((n) => nodePositions.has(n.id))
    .map((n) => ({
      id: n.id,
      x: nodePositions.get(n.id)!.x,
      y: nodePositions.get(n.id)!.y,
    }))
}
