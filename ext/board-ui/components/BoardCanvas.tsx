import React, { useCallback, useMemo, useRef, useState } from 'react'
import {
  ReactFlow,
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
  Node,
  Edge,
  OnNodeDrag,
  BackgroundVariant,
  EdgeChange,
  NodeChange,
  ConnectionMode,
  ConnectionLineComponentProps,
  getBezierPath,
} from '@xyflow/react'
import '@xyflow/react/dist/style.css'
import { getNodeIntersection } from '../util/edgeUtils'
import { useBoardStore } from '../store/boardStore'
import { BoardContext } from '../store/BoardContext'
import BoardNodeComponent from './BoardNode'
import BoardGroupNode from './BoardGroup'
import FloatingEdge from './FloatingEdge'
import { EdgeRelationMenu, RELATION_OPTIONS, RelationOption } from './EdgeRelationMenu'
import { PALETTE_DND_TYPE } from './NodePalette'
import { applyAutoLayout } from '../util/autoLayout'

// Preview line: origin = node-boundary intersection (same as FloatingEdge), end = cursor.
const ConnectionLine = ({ fromNode, toX, toY }: ConnectionLineComponentProps) => {
  if (!fromNode) return null
  const sp = getNodeIntersection(fromNode as any, toX, toY)
  const [path] = getBezierPath({ sourceX: sp.x, sourceY: sp.y, targetX: toX, targetY: toY })
  return (
    <g>
      <path d={path} fill="none" stroke="#4a5568" strokeWidth={2} strokeDasharray="6 4" strokeLinecap="round" />
      <circle cx={toX} cy={toY} r={3} fill="#4a5568" />
    </g>
  )
}

interface BoardCanvasProps {
  sendCommand: (command: string, data?: any) => void
}

// Defined outside component for stable references
const nodeTypes = { boardNode: BoardNodeComponent, groupNode: BoardGroupNode }
const edgeTypes = { floating: FloatingEdge }

interface PendingConnection {
  sourceId: string
  targetId: string
  x: number
  y: number
}

export const BoardCanvas = ({ sendCommand }: BoardCanvasProps) => {
  const {
    nodes: boardNodes,
    edges: boardEdges,
    groups: boardGroups,
    currentBoardId,
    viewport,
    followedNodeId,
    searchQuery,
  } = useBoardStore()

  // --- P1: Relation menu state ---
  const [pendingConnection, setPendingConnection] = useState<PendingConnection | null>(null)

  const boardNodeIds = useMemo(() => new Set(boardNodes.map((n) => n.id)), [boardNodes])

  const groupById = useMemo(() => {
    const map = new Map<string, (typeof boardGroups)[number]>()
    boardGroups.forEach((g) => map.set(g.id, g))
    return map
  }, [boardGroups])

  const nodeToGroupId = useMemo(() => {
    const map = new Map<string, string>()
    boardGroups.forEach((g) => {
      g.nodeIds.forEach((nodeId) => map.set(nodeId, g.id))
    })
    return map
  }, [boardGroups])

  const rfNodes: Node[] = useMemo(() => {
    const groups: Node[] = boardGroups.map((g) => ({
      id: `group-${g.id}`,
      type: 'groupNode',
      position: { x: g.x, y: g.y },
      data: { label: g.label, color: g.color },
      style: { width: g.width, height: g.height, zIndex: 0 },
      draggable: false,
      selectable: false,
    }))

    const cards: Node[] = boardNodes.map((n) => {
      const parentGroupId = nodeToGroupId.get(n.id)
      const parentGroup = parentGroupId ? groupById.get(parentGroupId) : undefined
      const isGrouped = Boolean(parentGroupId && parentGroup)
      const position = isGrouped && parentGroup
        ? { x: n.x - parentGroup.x, y: n.y - parentGroup.y }
        : { x: n.x, y: n.y }

      return {
        id: n.id,
        type: 'boardNode',
        position,
        parentId: isGrouped ? `group-${parentGroupId}` : undefined,
        extent: isGrouped ? 'parent' : undefined,
        data: {
          title: n.title,
          tags: n.tags,
          tagFields: n.tagFields || {},
          content: n.content || '',
          collapsed: n.collapsed,
          isFollowed: n.id === followedNodeId,
          onDoubleClick: (nodeId: string) => {
            sendCommand('open-node', { id: nodeId })
          },
          onDelete: (nodeId: string) => {
            if (!currentBoardId) return
            sendCommand('remove-node', { boardId: currentBoardId, nodeId })
          },
        },
        style: { width: n.width || 180 },
      }
    })

    return [...groups, ...cards]
  }, [
    boardGroups,
    boardNodes,
    nodeToGroupId,
    groupById,
    followedNodeId,
    sendCommand,
    currentBoardId,
  ])

  const rfEdges: Edge[] = useMemo(
    () =>
      boardEdges.map((e) => {
        const strokeColor = e.color || (e.isGlobal ? '#a0aec0' : '#4a5568')
        return {
          id: e.id,
          source: e.from,
          target: e.to,
          label: e.label || undefined,
          type: 'floating',
          data: { relationType: e.relationType },
          style: {
            stroke: strokeColor,
            strokeDasharray: e.style === 'dashed' ? '5,5' : undefined,
            strokeWidth: e.isGlobal ? 1 : 2,
          },
          animated: e.isGlobal,
        }
      }),
    [boardEdges]
  )

  const [nodes, setNodes, onNodesChange] = useNodesState(rfNodes)
  const [edges, setEdges, onEdgesChange] = useEdgesState(rfEdges)

  React.useEffect(() => { setNodes(rfNodes) }, [rfNodes])
  React.useEffect(() => { setEdges(rfEdges) }, [rfEdges])

  const onNodesChangeWithSync = useCallback(
    (changes: NodeChange[]) => {
      if (currentBoardId) {
        changes
          .filter((c) => c.type === 'remove')
          .forEach((c) => {
            const id = (c as any).id as string
            if (!boardNodeIds.has(id)) return
            sendCommand('remove-node', { boardId: currentBoardId, nodeId: id })
          })
      }
      onNodesChange(changes)
    },
    [currentBoardId, sendCommand, onNodesChange, boardNodeIds]
  )

  const onNodeDragStop: OnNodeDrag = useCallback(
    (_event, node) => {
      if (!currentBoardId || !boardNodeIds.has(node.id)) return
      const absX = (node as any).positionAbsolute?.x ?? node.position.x
      const absY = (node as any).positionAbsolute?.y ?? node.position.y

      sendCommand('move-node', {
        boardId: currentBoardId,
        nodeId: node.id,
        x: absX,
        y: absY,
      })

      const width = (node.width as number) || 180
      const height = (node.height as number) || 60
      const centerX = absX + width / 2
      const centerY = absY + height / 2

      const targetGroup = boardGroups.find(
        (g) =>
          centerX >= g.x &&
          centerX <= g.x + g.width &&
          centerY >= g.y &&
          centerY <= g.y + g.height
      )

      const fromGroupId = nodeToGroupId.get(node.id) || null
      const toGroupId = targetGroup?.id || null
      if (fromGroupId === toGroupId) return

      if (fromGroupId) {
        const fromGroup = groupById.get(fromGroupId)
        if (fromGroup) {
          sendCommand('update-group', {
            boardId: currentBoardId,
            groupId: fromGroupId,
            nodeIds: fromGroup.nodeIds.filter((id) => id !== node.id),
          })
        }
      }

      if (toGroupId && targetGroup) {
        const nextIds = targetGroup.nodeIds.includes(node.id)
          ? targetGroup.nodeIds
          : [...targetGroup.nodeIds, node.id]
        sendCommand('update-group', {
          boardId: currentBoardId,
          groupId: toGroupId,
          nodeIds: nextIds,
        })
      }
    },
    [currentBoardId, sendCommand, boardNodeIds, boardGroups, nodeToGroupId, groupById]
  )

  // Track source node across the connect drag lifecycle
  const connectingNodeId = useRef<string | null>(null)

  const onConnectStart = useCallback(
    (_event: any, { nodeId }: { nodeId: string | null }) => {
      connectingNodeId.current = nodeId
    },
    []
  )

  // On mouse-up, detect which card is under the cursor. Then show relation type menu.
  const onConnectEnd = useCallback(
    (event: MouseEvent | TouchEvent) => {
      const sourceId = connectingNodeId.current
      connectingNodeId.current = null
      if (!sourceId || !currentBoardId) return

      const clientX = 'touches' in event
        ? (event as TouchEvent).changedTouches[0].clientX
        : (event as MouseEvent).clientX
      const clientY = 'touches' in event
        ? (event as TouchEvent).changedTouches[0].clientY
        : (event as MouseEvent).clientY

      const elements = document.elementsFromPoint(clientX, clientY)
      const nodeEl = elements.find((el) => el.classList.contains('react-flow__node'))
      const targetId = nodeEl?.getAttribute('data-id') ?? null

      if (targetId && targetId !== sourceId && boardNodeIds.has(targetId)) {
        // Show relation type menu instead of immediately creating edge
        setPendingConnection({ sourceId, targetId, x: clientX, y: clientY })
      }
    },
    [currentBoardId, boardNodeIds]
  )

  // --- P1: Handle relation type selection ---
  const handleRelationSelect = useCallback(
    (relation: RelationOption) => {
      if (!pendingConnection || !currentBoardId) return
      sendCommand('add-edge', {
        boardId: currentBoardId,
        from: pendingConnection.sourceId,
        to: pendingConnection.targetId,
        label: '',
        relationType: relation.value,
      })
      setPendingConnection(null)
    },
    [currentBoardId, pendingConnection, sendCommand]
  )

  const handleRelationDismiss = useCallback(() => {
    setPendingConnection(null)
  }, [])

  // --- P1b: Auto-layout ---
  const handleAutoLayout = useCallback(() => {
    if (!currentBoardId || boardNodes.length === 0) return

    const layoutResult = applyAutoLayout(boardNodes, boardEdges)

    // Send new positions for each moved node
    layoutResult.forEach(({ id, x, y }) => {
      sendCommand('move-node', { boardId: currentBoardId, nodeId: id, x, y })
    })
  }, [currentBoardId, boardNodes, boardEdges, sendCommand])

  // Edges delete sync
  const onEdgesChangeWithSync = useCallback(
    (changes: EdgeChange[]) => {
      if (currentBoardId) {
        changes
          .filter((c) => c.type === 'remove')
          .forEach((c) => {
            sendCommand('remove-edge', { boardId: currentBoardId, edgeId: (c as any).id })
          })
      }
      onEdgesChange(changes)
    },
    [currentBoardId, sendCommand, onEdgesChange]
  )

  const onMoveEnd = useCallback(
    (_event: any, vp: { x: number; y: number; zoom: number }) => {
      if (!currentBoardId) return
      sendCommand('save-viewport', {
        boardId: currentBoardId,
        x: vp.x,
        y: vp.y,
        zoom: vp.zoom,
      })
    },
    [currentBoardId, sendCommand]
  )

  // DnD: accept drop from NodePalette
  const onDragOver = useCallback((event: React.DragEvent) => {
    if (event.dataTransfer.types.includes(PALETTE_DND_TYPE)) {
      event.preventDefault()
      event.dataTransfer.dropEffect = 'copy'
    }
  }, [])

  const reactFlowWrapper = React.useRef<HTMLDivElement>(null)

  const onDrop = useCallback(
    (event: React.DragEvent) => {
      event.preventDefault()
      const nodeId = event.dataTransfer.getData(PALETTE_DND_TYPE)
      if (!nodeId || !currentBoardId || !reactFlowWrapper.current) return

      const rfInstance = (reactFlowWrapper.current as any).__rfInstance
      if (!rfInstance) return

      const position = rfInstance.screenToFlowPosition({
        x: event.clientX,
        y: event.clientY,
      })

      sendCommand('add-node', {
        boardId: currentBoardId,
        nodeId,
        x: position.x,
        y: position.y,
      })
    },
    [currentBoardId, sendCommand]
  )

  const onInit = useCallback((instance: any) => {
    if (reactFlowWrapper.current) {
      (reactFlowWrapper.current as any).__rfInstance = instance
    }
  }, [])

  if (!currentBoardId) return null

  const contextValue = useMemo(
    () => ({ sendCommand, currentBoardId, searchQuery, onAutoLayout: handleAutoLayout }),
    [sendCommand, currentBoardId, searchQuery, handleAutoLayout]
  )

  return (
    <BoardContext.Provider value={contextValue}>
      <div
        ref={reactFlowWrapper}
        style={{ width: '100%', height: '100%' }}
        onDragOver={onDragOver}
        onDrop={onDrop}
      >
        <ReactFlow
          nodes={nodes}
          edges={edges}
          onNodesChange={onNodesChangeWithSync}
          onEdgesChange={onEdgesChangeWithSync}
          onNodeDragStop={onNodeDragStop}
          onConnectStart={onConnectStart}
          onConnectEnd={onConnectEnd as any}
          onMoveEnd={onMoveEnd}
          onInit={onInit}
          nodeTypes={nodeTypes}
          edgeTypes={edgeTypes}
          connectionMode={ConnectionMode.Loose}
          connectionLineComponent={ConnectionLine}
          defaultViewport={viewport}
          fitView={!viewport || (viewport.x === 0 && viewport.y === 0 && viewport.zoom === 1)}
          snapToGrid
          snapGrid={[16, 16]}
          minZoom={0.1}
          maxZoom={4}
          deleteKeyCode="Delete"
          connectOnClick={false}
          connectionRadius={0}
          nodeDragThreshold={3}
          nodesConnectable={true}
          nodesDraggable={true}
        >
          <Background variant={BackgroundVariant.Dots} gap={16} size={1} color="#e2e8f0" />
          <Controls />
          <MiniMap
            nodeStrokeWidth={3}
            zoomable
            pannable
            style={{ bottom: 16, right: 16 }}
          />
        </ReactFlow>
      </div>

      {/* P1: Relation type selection menu */}
      {pendingConnection && (
        <EdgeRelationMenu
          x={pendingConnection.x}
          y={pendingConnection.y}
          onSelect={handleRelationSelect}
          onDismiss={handleRelationDismiss}
        />
      )}
    </BoardContext.Provider>
  )
}
