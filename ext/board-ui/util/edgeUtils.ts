import { InternalNode } from '@xyflow/react'

/** 返回节点中心坐标（绝对位置）。 */
export function getNodeCenter(node: InternalNode): { x: number; y: number } {
  const w = node.measured?.width ?? (node as any).width ?? 180
  const h = node.measured?.height ?? (node as any).height ?? 60
  const pos = node.internals.positionAbsolute
  return { x: pos.x + w / 2, y: pos.y + h / 2 }
}

/**
 * 计算从节点中心出发、指向 (targetX, targetY) 的射线
 * 与节点矩形边框的交点（即浮动出线点）。
 */
export function getNodeIntersection(
  node: InternalNode,
  targetX: number,
  targetY: number,
): { x: number; y: number } {
  const w = node.measured?.width ?? (node as any).width ?? 180
  const h = node.measured?.height ?? (node as any).height ?? 60
  const pos = node.internals.positionAbsolute
  const cx = pos.x + w / 2
  const cy = pos.y + h / 2
  const hw = w / 2
  const hh = h / 2

  const dx = targetX - cx
  const dy = targetY - cy

  if (Math.abs(dx) < 0.001 && Math.abs(dy) < 0.001) {
    return { x: cx, y: cy }
  }

  // 射线参数 t：先撞到哪条边就用哪个 t
  const tx = Math.abs(dx) > 0.001 ? hw / Math.abs(dx) : Infinity
  const ty = Math.abs(dy) > 0.001 ? hh / Math.abs(dy) : Infinity
  const t = Math.min(tx, ty)

  return { x: cx + t * dx, y: cy + t * dy }
}
