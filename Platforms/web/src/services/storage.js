import Dexie from 'dexie'

const db = new Dexie('TimeWarpIDE')

db.version(1).stores({
  projects: '++id, userId',
  files: '++id, projectId, name',
  executions: '++id, projectId, createdAt',
  cache: '++id, key',
})

export const projectsDB = {
  add: (project) => db.projects.add(project),
  get: (id) => db.projects.get(id),
  getAll: () => db.projects.toArray(),
  getAllForUser: (userId) =>
    db.projects.where('userId').equals(userId).toArray(),
  update: (id, data) => db.projects.update(id, data),
  delete: (id) => db.projects.delete(id),
  clear: () => db.projects.clear(),
}

export const filesDB = {
  add: (file) => db.files.add(file),
  get: (id) => db.files.get(id),
  getAll: () => db.files.toArray(),
  getByProject: (projectId) =>
    db.files.where('projectId').equals(projectId).toArray(),
  update: (id, data) => db.files.update(id, data),
  delete: (id) => db.files.delete(id),
  deleteByProject: (projectId) =>
    db.files.where('projectId').equals(projectId).delete(),
  clear: () => db.files.clear(),
}

export const executionsDB = {
  add: (execution) => db.executions.add(execution),
  get: (id) => db.executions.get(id),
  getAll: () => db.executions.toArray(),
  getByProject: (projectId) =>
    db.executions.where('projectId').equals(projectId).toArray(),
  update: (id, data) => db.executions.update(id, data),
  delete: (id) => db.executions.delete(id),
  clear: () => db.executions.clear(),
}

export const cacheDB = {
  set: async (key, value) => {
    return db.cache.put({ key, value, timestamp: Date.now() })
  },
  get: async (key) => {
    const item = await db.cache.where('key').equals(key).first()
    return item?.value
  },
  delete: (key) => db.cache.where('key').equals(key).delete(),
  clear: () => db.cache.clear(),
}

export default db
