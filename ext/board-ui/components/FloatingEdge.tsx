import React, { useCallback, useEffect, useRef, useState } from 'react'
import {
  BaseEdge,
  EdgeLabelRenderer,
  EdgeProps,
  getBezierPath,
  useInternalNode,
} from '@xyflow/react'
import { getNodeCenter, getNodeIntersection } from '../util/edgeUtils'
import { useBoardContext } from '../store/BoardContext'

// Relation type display config — mirrors RELATION_OPTIONS in EdgeRelationMenu.tsx
const RELATION_META: Record<string, { label: string; color: string }> = {
  reference:  { label: 'Reference',  color: '#4a5568' },
  parent:     { label: 'Parent→Child', color: '#2b6cb0' },
  related:    { label: 'Related',   color: '#276749' },
  conflicts:  { label: 'Conflicts', color: '#c53030' },
}

interface FloatingEdgeData {
  relationType?: string
}

const FloatingEdge = ({
  id,
  source,
  target,
  label,
  style,
  markerEnd,
  selected,
  data,
}: EdgeProps) => {
  const sourceNode = useInternalNode(source)
  const targetNode = useInternalNode(target)
  const { sendCommand, currentBoardId } = useBoardContext()

  const [isEditing, setIsEditing] = useState(false)
  const [isHovered, setIsHovered] = useState(false)
  const [labelValue, setLabelValue] = useState((label as string) ?? '')
  const inputRef = useRef<HTMLInputElement>(null)

  // Extract relation type from edge data
  const relationType: string | undefined = (data as FloatingEdgeData | undefined)?.relationType
  const relation = relationType ? RELATION_META[relationType] : undefined

  // Sync external label changes
  useEffect(() => {
    setLabelValue((label as string) ?? '')
  }, [label])

  // Focus input on edit mode
  useEffect(() => {
    if (isEditing) inputRef.current?.focus()
  }, [isEditing])

  const saveLabel = useCallback(() => {
    setIsEditing(false)
    if (!currentBoardId) return
    sendCommand('update-edge', { boardId: currentBoardId, edgeId: id, label: labelValue })
  }, [currentBoardId, id, labelValue, sendCommand])

  const cancelEdit = useCallback(() => {
    setIsEditing(false)
    setLabelValue((label as string) ?? '')
  }, [label])

  const handleDelete = useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      if (!currentBoardId) return
      sendCommand('remove-edge', { boardId: currentBoardId, edgeId: id })
    },
    [currentBoardId, id, sendCommand],
  )

  if (!sourceNode || !targetNode) return null

  const sc = getNodeCenter(sourceNode)
  const tc = getNodeCenter(targetNode)

  const sp = getNodeIntersection(sourceNode, tc.x, tc.y)
  const tp = getNodeIntersection(targetNode, sc.x, sc.y)

  const [edgePath, labelX, labelY] = getBezierPath({
    sourceX: sp.x,
    sourceY: sp.y,
    targetX: tp.x,
    targetY: tp.y,
  })

  const hasLabel = labelValue.trim().length > 0
  const showControls = selected || isHovered
  const hasRelation = Boolean(relation)

  return (
    <>
      <BaseEdge id={id} path={edgePath} style={style} markerEnd={markerEnd} interactionWidth={20} />

      <EdgeLabelRenderer>
        {/* Relation type badge (shown above the edge label / on the edge line when no label) */}
        {hasRelation && (
          <div
            style={{
              position: 'absolute',
              transform: `translate(-50%, -50%) translate(${labelX}px,${labelY + (hasLabel ? -18 : 0)}px)`,
              pointerEvents: 'none',
              opacity: showControls ? 1 : 0.55,
              transition: 'opacity 0.15s',
              fontSize: 9,
              fontWeight: 600,
              color: relation!.color,
              background: `${relation!.color}15`,
              border: `1px solid ${relation!.color}40`,
              borderRadius: 3,
              padding: '1px 5px',
              whiteSpace: 'nowrap',
              letterSpacing: '0.02em',
              textTransform: 'uppercase',
            }}
            className="nodrag nopan"
          >
            {relation!.label}
          </div>
        )}

        {/* Main label area (editable text + delete button) */}
        <div
          style={{
            position: 'absolute',
            transform: `translate(-50%, -50%) translate(${labelX}px,${labelY + (hasRelation ? 10 : 0)}px)`,
            pointerEvents: 'all',
            minWidth: 24,
            minHeight: 24,
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
          className="nodrag nopan"
          onMouseEnter={() => setIsHovered(true)}
          onMouseLeave={() => setIsHovered(false)}
        >
          {isEditing ? (
            <input
              ref={inputRef}
              value={labelValue}
              onChange={(e) => setLabelValue(e.target.value)}
              onBlur={saveLabel}
              onKeyDown={(e) => {
                if (e.key === 'Enter') saveLabel()
                if (e.key === 'Escape') cancelEdit()
              }}
              style={{
                background: 'white',
                border: '1px solid #4a5568',
                borderRadius: 4,
                padding: '2px 6px',
                fontSize: 11,
                outline: 'none',
                minWidth: 60,
              }}
            />
          ) : (
            <div
              onDoubleClick={() => setIsEditing(true)}
              style={{
                display: 'flex',
                alignItems: 'center',
                gap: 3,
                background: hasLabel || showControls ? 'white' : 'transparent',
                border: hasLabel || showControls ? '1px solid #e2e8f0' : 'none',
                borderRadius: 4,
                padding: hasLabel || showControls ? '2px 5px' : 0,
                fontSize: 11,
                color: '#4a5568',
                cursor: 'default',
                userSelect: 'none',
                whiteSpace: 'nowrap',
                position: 'relative',
              }}
            >
              {hasLabel && <span>{labelValue}</span>}
              {showControls && (
                <button
                  onClick={handleDelete}
                  title="Delete line"
                  style={{
                    background: '#fc8181',
                    color: 'white',
                    border: 'none',
                    borderRadius: '50%',
                    width: 14,
                    height: 14,
                    fontSize: 10,
                    cursor: 'pointer',
                    padding: 0,
                    lineHeight: '14px',
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                    flexShrink: 0,
                  }}
                >
                  ×
                </button>
              )}
            </div>
          )}
        </div>
      </EdgeLabelRenderer>
    </>
  )
}

export default FloatingEdge
