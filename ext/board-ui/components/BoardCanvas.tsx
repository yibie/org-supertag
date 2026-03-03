import React, { useCallback, useMemo, useRef } from 'react'
import {
  ReactFlow,
  Background,
  Controls,
  MiniMap,
  useNodesState,
  useEdgesState,
  Node,
  Edge,
  Connection,
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
import { PALETTE_DND_TYPE } from './NodePalette'

// Preview line: origin = node-boundary intersection (same as FloatingEdge), end = cursor.
// Uses fromNode so the line always exits from the card boundary toward the cursor,
// regardless of which bar handle the user grabbed.
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

export const BoardCanvas = ({ sendCommand }: BoardCanvasProps) => {
  const {
    nodes: boardNodes,
    edges: boardEdges,
    groups: boardGroups,
    currentBoardId,
    viewport,
    followedNodeId,
  } =
    useBoardStore()
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

  // On mouse-up, detect which card is under the cursor via DOM hit-test.
  // This bypasses ReactFlow's handle-snap (connectionRadius=0) so the preview
  // line follows the cursor freely and the user can drop anywhere on a card.
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

      // Walk elements at drop position from topmost downward; find first node
      const elements = document.elementsFromPoint(clientX, clientY)
      const nodeEl = elements.find((el) => el.classList.contains('react-flow__node'))
      const targetId = nodeEl?.getAttribute('data-id') ?? null

      if (targetId && targetId !== sourceId && boardNodeIds.has(targetId)) {
        sendCommand('add-edge', {
          boardId: currentBoardId,
          from: sourceId,
          to: targetId,
          label: '',
        })
      }
    },
    [currentBoardId, sendCommand, boardNodeIds]
  )

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

      // Convert screen drop position to flow coordinates
      // We need the ReactFlow instance here — access it via the context we already expose
      // onDrop fires on the wrapper div; use clientX/Y relative to the wrapper
      // We'll attach the instance via a forwarded ref pattern using a small trick:
      // store it on the wrapper element during onInit
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
    () => ({ sendCommand, currentBoardId }),
    [sendCommand, currentBoardId]
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

    </BoardContext.Provider>
  )
}
